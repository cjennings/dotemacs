#!/usr/bin/env bats
# Tests for the helpers in scripts/setup-telega.sh.
#
# Sources the script (so only the helpers come in -- `main' runs only
# when the script is executed directly) and stubs `docker' / `command'
# in each test so nothing actually talks to the docker daemon or
# requires a real telega-server image.

setup() {
    source "${BATS_TEST_DIRNAME}/../scripts/setup-telega.sh"
}

# --------------------------- ensure_docker_installed ----------------------

@test "ensure_docker_installed: succeeds when docker is on PATH" {
    command() {
        if [[ "$1" == "-v" && "$2" == "docker" ]]; then
            echo "/usr/bin/docker"
            return 0
        fi
        builtin command "$@"
    }
    run ensure_docker_installed
    [ "$status" -eq 0 ]
    [[ "$output" == *"docker present"* ]]
}

@test "ensure_docker_installed: fails when docker is missing" {
    command() {
        if [[ "$1" == "-v" && "$2" == "docker" ]]; then
            return 1
        fi
        builtin command "$@"
    }
    run ensure_docker_installed
    [ "$status" -eq 1 ]
    [[ "$output" == *"docker not found"* ]]
    [[ "$output" == *"get-docker"* ]]
}

# --------------------------- ensure_docker_running ------------------------

@test "ensure_docker_running: succeeds when 'docker info' exits 0" {
    docker() { return 0; }
    run ensure_docker_running
    [ "$status" -eq 0 ]
    [[ "$output" == *"reachable"* ]]
}

@test "ensure_docker_running: fails when 'docker info' errors" {
    docker() { return 1; }
    run ensure_docker_running
    [ "$status" -eq 1 ]
    [[ "$output" == *"cannot talk"* ]]
    [[ "$output" == *"docker group"* || "$output" == *"systemctl"* ]]
}

# --------------------------- pull_or_announce_image -----------------------

@test "pull_or_announce_image: announces the in-Emacs build when no image is set" {
    TELEGA_DOCKER_IMAGE=""
    run pull_or_announce_image
    [ "$status" -eq 0 ]
    [[ "$output" == *"M-x telega-server-build"* ]]
}

@test "pull_or_announce_image: pulls when TELEGA_DOCKER_IMAGE is set" {
    TELEGA_DOCKER_IMAGE="example/telega:1"
    docker() {
        if [[ "$1" == "pull" ]]; then
            echo "pulling $2"
            return 0
        fi
        return 1
    }
    run pull_or_announce_image
    [ "$status" -eq 0 ]
    [[ "$output" == *"pulled example/telega:1"* ]]
}

@test "pull_or_announce_image: fails when 'docker pull' errors" {
    TELEGA_DOCKER_IMAGE="example/telega:1"
    docker() { return 1; }
    run pull_or_announce_image
    [ "$status" -eq 1 ]
    [[ "$output" == *"pull failed"* ]]
}
