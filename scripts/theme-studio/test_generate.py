"""Tests for the theme-studio page generator (generate.py).

The generator's risky logic is the export-strip and the placeholder substitution
that inline colormath.js and the sample/palette data into the page. A bug there
ships a broken theme-studio.html that the JS unit tests can't see. These tests
exercise the strip in isolation and assert the assembled page has every
placeholder filled and carries the colormath body verbatim.

Run: python3 -m unittest test_generate   (from scripts/theme-studio/)
"""
import os
import unittest

import generate  # importable without side effects: the file write is __main__-guarded


class StripExports(unittest.TestCase):
    def test_removes_the_export_line_keeps_the_body(self):
        src = "function f(){return 1;}\nexport { f };"
        self.assertEqual(generate.strip_exports(src), "function f(){return 1;}")

    def test_preserves_multiline_body_and_rstrips_trailing_blanks(self):
        src = "const a=1;\nconst b=2;\nexport { a, b };\n\n"
        self.assertEqual(generate.strip_exports(src), "const a=1;\nconst b=2;")

    def test_no_export_line_returns_body_rstripped(self):
        self.assertEqual(generate.strip_exports("let x=1;\n"), "let x=1;")

    def test_removes_every_export_line_not_just_the_last(self):
        src = "export const a=1;\ncode();\nexport { a };"
        self.assertEqual(generate.strip_exports(src), "code();")

    def test_matches_the_js_side_strip_so_integrity_holds(self):
        # test-colormath.mjs strips with the same rule: drop lines starting with
        # 'export', then trim trailing whitespace. Keep the two in lockstep.
        src = "x();\nexport { x };\n"
        js_equivalent = "\n".join(
            l for l in src.split("\n") if not l.startswith("export")
        ).rstrip()
        self.assertEqual(generate.strip_exports(src), js_equivalent)


class ColormathInlining(unittest.TestCase):
    def setUp(self):
        self.cm_src = open(os.path.join(generate.HERE, "colormath.js")).read()

    def test_colormath_export_is_a_single_line(self):
        # The strip is line-based, so a multi-line `export { ... }` would leave the
        # continuation lines behind as a dangling block (a real bug this caught).
        export_lines = [l for l in self.cm_src.splitlines() if l.startswith("export")]
        self.assertEqual(len(export_lines), 1, "colormath.js must have one export line")

    def test_stripped_body_has_no_export_line_and_ends_cleanly(self):
        # "export" can still appear inside a comment; what must be gone is any line
        # that *starts* with export (and the dangling continuation lines a
        # multi-line export would leave).
        body = generate.strip_exports(self.cm_src)
        for line in body.splitlines():
            self.assertFalse(line.startswith("export"), f"export line survived: {line!r}")
        self.assertTrue(body.endswith("}"), "body should end at the last function")


class AssembledPage(unittest.TestCase):
    PLACEHOLDERS = [
        "STYLES_CSS", "APP_JS",
        "COLORMATH_J", "SAMPLES_J", "PALETTE_J", "CATS_J",
        "UIFACES_J", "UIMAP_J", "APPS_J", "BOLD_J", "MAP_J",
    ]

    def test_every_placeholder_is_substituted(self):
        for token in self.PLACEHOLDERS:
            self.assertNotIn(token, generate.HTML, f"{token} left unsubstituted")

    def test_page_carries_the_colormath_body_verbatim(self):
        # Python-side inline-integrity: the same guarantee the JS test asserts, but
        # checked at the point the page is built rather than after a round-trip.
        self.assertIn(generate.COLORMATH_BODY, generate.HTML)

    def test_page_carries_the_stylesheet_verbatim(self):
        # styles.css has no placeholders, so it inlines verbatim: the inlined copy
        # and the source file cannot drift.
        self.assertIn(generate.STYLES, generate.HTML)

    def test_page_carries_the_app_script_faithfully(self):
        # app.js does carry placeholders, so the page holds it as fill_data renders
        # it (APP_FILLED), not the raw file. This guards the splice: the script
        # reaches the page intact, with its data placeholders correctly filled.
        self.assertIn(generate.APP_FILLED, generate.HTML)

    def test_page_is_a_single_script_document(self):
        self.assertEqual(generate.HTML.count("<script>"), 1)
        self.assertEqual(generate.HTML.count("</script>"), 1)


if __name__ == "__main__":
    unittest.main()
