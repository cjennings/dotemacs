// Shared by the inline-integrity tests (test-colormath.mjs, test-app-core.mjs,
// test-app-util.mjs). Mirrors strip_exports in generate.py: drop top-level
// export/import lines (a pure module may import a peer for its own unit tests,
// while the inlined page copy relies on that peer already being present), then
// rstrip. The page is asserted to carry the stripped body verbatim, so this MUST
// stay aligned with generate.py's strip_exports -- one definition keeps the three
// test copies from drifting apart.
//
// (This file matches the `*.mjs` test glob in run-tests.sh; it carries no tests,
// so it contributes zero to the count.)
export const stripInlinedBody = (s) =>
  s.split('\n')
    .filter((l) => !(l.startsWith('export') || l.startsWith('import')))
    .join('\n')
    .replace(/\s+$/, '');
