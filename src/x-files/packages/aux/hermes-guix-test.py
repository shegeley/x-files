"""Offline contracts for Guix packaging and credential-free Codex workers."""

from argparse import Namespace
import importlib

import pytest


@pytest.fixture
def isolated_config(tmp_path, monkeypatch):
    import hermes_cli.config as config
    from hermes_cli import managed_scope

    home = tmp_path / "home"
    home.mkdir()
    monkeypatch.setenv("HERMES_HOME", str(home))
    monkeypatch.setenv("HERMES_MANAGED_DIR", str(tmp_path / "absent"))
    monkeypatch.delenv("HERMES_MANAGED", raising=False)
    for key in ("OPENAI_API_KEY", "OPENROUTER_API_KEY", "ANTHROPIC_API_KEY"):
        monkeypatch.delenv(key, raising=False)
    config._LOAD_CONFIG_CACHE.clear()
    config._RAW_CONFIG_CACHE.clear()
    managed_scope.invalidate_managed_cache()
    yield home
    config._LOAD_CONFIG_CACHE.clear()
    config._RAW_CONFIG_CACHE.clear()
    managed_scope.invalidate_managed_cache()


@pytest.mark.parametrize("provider", ["openai", "openai-codex"])
def test_codex_login_does_not_resolve_api_credentials(isolated_config, monkeypatch, provider):
    from hermes_cli import runtime_provider

    (isolated_config / "config.yaml").write_text(
        f"model:\n  provider: {provider}\n  openai_runtime: codex_app_server\n"
    )

    def unexpected_credentials(*args, **kwargs):
        pytest.fail("Codex CLI mode entered the API credential resolver")

    monkeypatch.setattr(runtime_provider, "_ladder_rungs", unexpected_credentials)
    runtime = runtime_provider.resolve_runtime_provider(requested=provider)
    assert runtime["api_mode"] == "codex_app_server"
    assert runtime["api_key"] is None
    assert runtime["base_url"] is None


def test_codex_agent_turn_without_api_client(isolated_config, monkeypatch):
    import run_agent
    from agent import agent_init
    from agent.transports.codex_app_server_session import CodexAppServerSession, TurnResult

    def unexpected_client(*args, **kwargs):
        pytest.fail("Codex CLI mode constructed an OpenAI API client")

    monkeypatch.setattr(agent_init, "_init_openai_client", unexpected_client)
    monkeypatch.setattr(CodexAppServerSession, "ensure_started", lambda self: "guix-thread")
    monkeypatch.setattr(
        CodexAppServerSession, "run_turn",
        lambda self, user_input, **kwargs: TurnResult(
            final_text=user_input,
            projected_messages=[{"role": "assistant", "content": user_input}],
            thread_id="guix-thread", turn_id="guix-turn",
        ),
    )
    agent = run_agent.AIAgent(
        provider="openai-codex", api_mode="codex_app_server",
        api_key=None, base_url=None, quiet_mode=True,
        skip_context_files=True, skip_memory=True,
    )
    monkeypatch.setattr(agent, "_spawn_background_review", lambda *args, **kwargs: None)
    assert agent.client is None
    result = agent.run_conversation("offline Codex turn")
    assert result["completed"] is True
    assert result["final_response"] == "offline Codex turn"
    assert result["codex_thread_id"] == "guix-thread"


def test_disabled_codex_provider_is_still_rejected(isolated_config):
    from hermes_cli import runtime_provider

    (isolated_config / "config.yaml").write_text(
        "model:\n  openai_runtime: codex_app_server\n"
        "providers:\n  openai-codex:\n    enabled: false\n"
    )
    with pytest.raises(ValueError, match="disabled"):
        runtime_provider.resolve_runtime_provider(requested="openai-codex")


def test_callback_is_worker_scoped_and_keeps_guix_paths(isolated_config, monkeypatch, tmp_path):
    import io
    import os
    import tomllib
    from unittest.mock import MagicMock
    from agent.transports import codex_app_server

    (isolated_config / "config.yaml").write_text("model:\n  codex_hermes_tools: true\n")
    codex_home = tmp_path / "codex"
    codex_home.mkdir()
    user_config = codex_home / "config.toml"
    original = '[mcp_servers.personal]\ncommand = "my-server"\n'
    user_config.write_text(original)
    monkeypatch.setenv("CODEX_HOME", str(codex_home))
    monkeypatch.setenv("GUIX_PYTHONPATH", "/store/hermes/site-packages:/store/deps/site-packages")
    process = MagicMock(stdin=io.BytesIO(), stdout=io.BytesIO(), stderr=io.BytesIO())
    spawn = MagicMock(return_value=process)
    monkeypatch.setattr(codex_app_server.subprocess, "Popen", spawn)
    client = codex_app_server.CodexAppServerClient()
    client._closed = True
    client._reader.join(timeout=2)
    client._stderr_reader.join(timeout=2)
    argv = spawn.call_args.args[0]
    settings = tomllib.loads("\n".join(argv[i + 1] for i, value in enumerate(argv) if value == "-c"))
    callback = settings["mcp_servers"]["hermes-tools"]
    assert callback["startup_timeout_sec"] >= 120
    assert callback["required"] is True
    assert callback["args"] == ["-m", "agent.transports.hermes_tools_mcp_server"]
    assert callback["env"]["GUIX_PYTHONPATH"] == os.environ["GUIX_PYTHONPATH"]
    assert callback["env"]["HERMES_HOME"] == str(isolated_config)
    assert "OPENAI_API_KEY" not in callback["env"]
    assert user_config.read_text() == original
    response = MagicMock()
    response.get.return_value = {"result": {}}
    monkeypatch.setattr(codex_app_server.queue, "Queue", lambda **_: response)
    monkeypatch.setattr(client, "_send", MagicMock())
    for method, timeout in (("thread/start", 130), ("thread/resume", 130), ("turn/start", 15)):
        client.request(method, timeout=15)
        assert response.get.call_args.kwargs["timeout"] == timeout


@pytest.mark.parametrize("module", ["acp", "anthropic", "mcp", "mcp.types", "httpx2"])
def test_protocol_dependencies_import(module):
    importlib.import_module(module)


def test_anthropic_client_constructs_without_network():
    from anthropic import Anthropic

    with Anthropic(api_key="offline-test-placeholder") as client:
        assert client.messages is not None


def test_guix_stamp_refuses_self_update_but_allows_user_config(isolated_config, tmp_path, monkeypatch, capsys):
    from hermes_cli import config, main

    code = tmp_path / "code"
    code.mkdir()
    (code / ".install_method").write_text("guix\n")
    monkeypatch.setattr(config, "get_project_root", lambda: code)
    assert config.detect_install_method() == "guix"
    assert main._update_preflight_handled(Namespace()) is True
    assert "Guix" in capsys.readouterr().out
    assert not config.is_managed()
    config.set_config_value("display.show_reasoning", "true")
    assert config.load_config()["display"]["show_reasoning"] is True


@pytest.mark.asyncio
async def test_callback_mcp_stdio_roundtrip(isolated_config):
    import os
    import sys
    from mcp import ClientSession
    from mcp.client.stdio import StdioServerParameters, stdio_client

    (isolated_config / "config.yaml").write_text("toolsets:\n  - hermes-cli\n  - kanban\n")
    parameters = StdioServerParameters(
        command=sys.executable,
        args=["-m", "agent.transports.hermes_tools_mcp_server"],
        env=dict(os.environ, HERMES_QUIET="1"),
    )
    async with stdio_client(parameters) as streams:
        async with ClientSession(*streams, read_timeout_seconds=120) as session:
            await session.initialize()
            tools = await session.list_tools()
            assert {"skills_list", "skill_view", "kanban_list"} <= {tool.name for tool in tools.tools}
            result = await session.call_tool("skills_list", {})
            assert not result.is_error


def test_managed_generation_switch_and_rollback_preserves_user_file(isolated_config, tmp_path, monkeypatch):
    from hermes_cli import config, managed_scope

    user_file = isolated_config / "config.yaml"
    original = "model: legacy-model\ndisplay:\n  show_reasoning: true\n"
    user_file.write_text(original)
    generations = []
    for number in (1, 2):
        directory = tmp_path / f"generation-{number}"
        directory.mkdir()
        (directory / "config.yaml").write_text(
            "model:\n  provider: openai-codex\n  openai_runtime: codex_app_server\n"
            f"  codex_bin: /store/generation-{number}/bin/codex\n"
            "security:\n  allow_lazy_installs: false\n"
        )
        generations.append(directory)
    for directory in (*generations, generations[0]):
        monkeypatch.setenv("HERMES_MANAGED_DIR", str(directory))
        managed_scope.invalidate_managed_cache()
        config._LOAD_CONFIG_CACHE.clear()
        config._RAW_CONFIG_CACHE.clear()
        loaded = config.load_config()
        assert directory.name in loaded["model"]["codex_bin"]
        assert loaded["model"]["openai_runtime"] == "codex_app_server"
        assert loaded["display"]["show_reasoning"] is True
        assert loaded["security"]["allow_lazy_installs"] is False
        from tools.lazy_deps import _allow_lazy_installs
        assert not _allow_lazy_installs()
        with pytest.raises(SystemExit):
            config.set_config_value("model.openai_runtime", "chat_completions")
        assert user_file.read_text() == original
