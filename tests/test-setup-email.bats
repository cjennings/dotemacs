#!/usr/bin/env bats
# Tests for the password helpers in scripts/setup-email.sh.
#
# `install_encrypted_password' copies a password file from the encrypted
# assets dir into PASSWORD_DEST_DIR; `decrypt_password' pipes one through
# `gpg -d' into PASSWORD_DEST_DIR.  Both skip when the destination already
# exists and exit 1 when the source is missing.  These tests source the
# script (which only defines the helpers — `main' runs only when the script
# is executed directly) and point the two directory vars at a per-test
# tmpdir, so nothing touches ~/.config or the real mail setup.

setup() {
    source "${BATS_TEST_DIRNAME}/../scripts/setup-email.sh"
    ENCRYPTED_PASSWORDS_DIR="${BATS_TEST_TMPDIR}/src"
    PASSWORD_DEST_DIR="${BATS_TEST_TMPDIR}/dest"
    mkdir -p "$ENCRYPTED_PASSWORDS_DIR" "$PASSWORD_DEST_DIR"
}

# --------------------------- install_encrypted_password ---------------------

@test "install_encrypted_password: copies the source and locks it to 600" {
    printf 'secret' > "$ENCRYPTED_PASSWORDS_DIR/.gmailpass.gpg"
    run install_encrypted_password ".gmailpass.gpg"
    [ "$status" -eq 0 ]
    [ "$(cat "$PASSWORD_DEST_DIR/.gmailpass.gpg")" = "secret" ]
    [ "$(stat -c '%a' "$PASSWORD_DEST_DIR/.gmailpass.gpg")" = "600" ]
    [[ "$output" == *"created"* ]]
}

@test "install_encrypted_password: skips and keeps an existing destination" {
    printf 'new'  > "$ENCRYPTED_PASSWORDS_DIR/.gmailpass.gpg"
    printf 'kept' > "$PASSWORD_DEST_DIR/.gmailpass.gpg"
    run install_encrypted_password ".gmailpass.gpg"
    [ "$status" -eq 0 ]
    [ "$(cat "$PASSWORD_DEST_DIR/.gmailpass.gpg")" = "kept" ]
    [[ "$output" == *"already exists, skipping"* ]]
}

@test "install_encrypted_password: exits 1 when source and destination both missing" {
    run install_encrypted_password ".gmailpass.gpg"
    [ "$status" -eq 1 ]
    [[ "$output" == *"missing"* ]]
    [ ! -e "$PASSWORD_DEST_DIR/.gmailpass.gpg" ]
}

# ------------------------------- decrypt_password ---------------------------

@test "decrypt_password: writes the decrypted plaintext and locks it to 600" {
    printf 'ciphertext' > "$ENCRYPTED_PASSWORDS_DIR/.cmailpass.gpg"
    gpg() { printf 'plaintext'; }                 # stub: no real GPG key here
    run decrypt_password ".cmailpass.gpg" ".cmailpass"
    [ "$status" -eq 0 ]
    [ "$(cat "$PASSWORD_DEST_DIR/.cmailpass")" = "plaintext" ]
    [ "$(stat -c '%a' "$PASSWORD_DEST_DIR/.cmailpass")" = "600" ]
    [[ "$output" == *"created"* ]]
}

@test "decrypt_password: skips and keeps an existing destination" {
    printf 'ciphertext' > "$ENCRYPTED_PASSWORDS_DIR/.cmailpass.gpg"
    printf 'kept'       > "$PASSWORD_DEST_DIR/.cmailpass"
    gpg() { printf 'plaintext'; }
    run decrypt_password ".cmailpass.gpg" ".cmailpass"
    [ "$status" -eq 0 ]
    [ "$(cat "$PASSWORD_DEST_DIR/.cmailpass")" = "kept" ]
    [[ "$output" == *"already exists, skipping"* ]]
}

@test "decrypt_password: exits 1 when the source is missing" {
    run decrypt_password ".cmailpass.gpg" ".cmailpass"
    [ "$status" -eq 1 ]
    [[ "$output" == *"missing"* ]]
    [ ! -e "$PASSWORD_DEST_DIR/.cmailpass" ]
}

@test "decrypt_password: removes the partial file and exits 1 when gpg fails" {
    printf 'ciphertext' > "$ENCRYPTED_PASSWORDS_DIR/.cmailpass.gpg"
    gpg() { return 1; }                           # stub: decryption failure
    run decrypt_password ".cmailpass.gpg" ".cmailpass"
    [ "$status" -eq 1 ]
    [[ "$output" == *"failed to decrypt"* ]]
    [ ! -e "$PASSWORD_DEST_DIR/.cmailpass" ]
}
