#!/usr/bin/env bash
set -euo pipefail

job_state="$HOME/job-$CI_JOB_ID"
if [[ ${1:-} == after ]]; then
    printf 'after_script\n' > "$job_state.after"
    exit 0
fi

trap 'printf "interrupted\n" > "$job_state.interrupted"; exit 1' HUP INT TERM QUIT
printf '%s\n' "$$" > "$job_state.pid"
id -u > "$job_state.uid"
{
    for tool in git guix hello; do command -v "$tool" || true; done
    printf 'RUNNER_FIXTURE=%s\n' "${RUNNER_FIXTURE:-}"
} > "$job_state.tools"
touch "$job_state.ready"
printf 'fixture job %s started\n' "$CI_JOB_ID"

for ((attempt = 0; attempt < 600; attempt++)); do
    if [[ -e /tmp/gitlab-runner-fixture/release-$CI_JOB_ID ]]; then
        printf 'fixture job %s completed\n' "$CI_JOB_ID" | tee "$job_state.done"
        if [[ $CI_JOB_ID == 102 ]]; then exit 7; fi
        exit 0
    fi
    sleep 0.1
done
printf 'fixture release timed out\n' >&2
exit 1
