"""rass preset for bun-mode: TypeScript answers, Biome formats.

Servers come from the command line (first = primary = TypeScript).

- rass sends formatting to the first capable server, which would be
  TypeScript; send it to Biome instead, falling back to the default.
- rass pulls diagnostics with ``"previousResultId": null``; the LSP spec
  types it as an optional string and TypeScript 7 rejects the null, so no
  type errors would ever show.  Drop it unless it is a string.
"""

from rassumfrassum.frassum import LspLogic

FORMATTING = {
    'textDocument/formatting': 'documentFormattingProvider',
    'textDocument/rangeFormatting': 'documentRangeFormattingProvider',
}


def spec_params(method, params):
    """PARAMS for METHOD without values the LSP spec does not allow."""
    if (method == 'textDocument/diagnostic'
            and not isinstance(params.get('previousResultId'), str)):
        return {k: v for k, v in params.items() if k != 'previousResultId'}
    return params


class BunLogic(LspLogic):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        request_server = self.request_server

        async def spec_request_server(server, method, params):
            return await request_server(server, method, spec_params(method, params))

        self.request_server = spec_request_server

    async def on_client_request(self, method, params, servers):
        if cap := FORMATTING.get(method):
            biome = [s for s in servers
                     if s.name.startswith('biome') and s.caps.get(cap)]
            if biome:
                return biome[:1]
        return await super().on_client_request(method, params, servers)


def servers():
    return []


def logic_class():
    return BunLogic
