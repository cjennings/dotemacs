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
from collections import Counter, defaultdict

import generate  # importable without side effects: the file write is __main__-guarded
from app_inventory import face_rows
from default_faces import DefaultFaces, changed_summary
from face_specs import package_face_spec, ui_face_spec


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

    def test_removes_import_lines_too(self):
        # A pure module may import a peer for its own tests; the import must be
        # stripped on inline (the peer is already in the page).
        src = "import { rl } from './colormath.js';\nfunction f(){return rl();}"
        self.assertEqual(generate.strip_exports(src), "function f(){return rl();}")

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
        "STYLES_CSS", "APP_JS", "APP_CORE_J", "APP_UTIL_J",
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

    def test_page_carries_the_app_core_body_verbatim(self):
        # app-core.js inlines verbatim (no data placeholders), so the inlined copy
        # and the unit-tested module cannot drift.
        self.assertIn(generate.APP_CORE_BODY, generate.HTML)

    def test_page_carries_the_app_util_body_verbatim(self):
        # app-util.js inlines verbatim after its import line is stripped.
        self.assertIn(generate.APP_UTIL_BODY, generate.HTML)

    def test_app_util_inlined_body_has_no_import_line(self):
        # The `import rl` line must be gone, or the page <script> is invalid.
        for line in generate.APP_UTIL_BODY.splitlines():
            self.assertFalse(line.startswith("import"), f"import survived: {line!r}")

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


class FacesHelper(unittest.TestCase):
    def test_strips_prefix_and_derives_label_and_merges_seed(self):
        # Normal: the prefix comes off the label, and the per-face seed is attached.
        rows = face_rows(["org-todo", "org-done"], "org-", {"org-todo": {"fg": "gold"}})
        self.assertEqual(rows, [
            ["org-todo", "todo", {"fg": "gold"}],
            ["org-done", "done", {}],
        ])

    def test_label_drops_face_suffix_and_spaces_remaining_dashes(self):
        # Boundary: "-face" is removed and the rest of the dashes become spaces.
        rows = face_rows(["lsp-rename-placeholder-face"], "lsp-", {})
        self.assertEqual(rows[0][1], "rename placeholder")

    def test_name_without_the_prefix_is_left_intact(self):
        # Boundary: a name that doesn't start with the prefix keeps its full text
        # (only "-face" removal and dash-spacing apply).
        rows = face_rows(["shr-text"], "org-", {})
        self.assertEqual(rows[0], ["shr-text", "shr text", {}])

    def test_empty_names_gives_empty_list(self):
        # Error/Boundary: nothing in, nothing out.
        self.assertEqual(face_rows([], "org-", {"org-todo": {"fg": "gold"}}), [])


class FaceSpecDefaults(unittest.TestCase):
    def test_ui_face_spec_fills_style_fields(self):
        self.assertEqual(ui_face_spec({"bg": "#ffffff", "bold": True}), {
            "fg": None,
            "bg": "#ffffff",
            "bold": True,
            "italic": False,
            "underline": False,
            "strike": False,
        })

    def test_package_face_spec_fills_structure_fields(self):
        self.assertEqual(package_face_spec({"inherit": "base", "height": 1.2}), {
            "fg": None,
            "bg": None,
            "bold": False,
            "italic": False,
            "underline": False,
            "strike": False,
            "inherit": "base",
            "height": 1.2,
            "box": None,
        })

    def test_generated_color_names_are_base_columns_when_legacy(self):
        self.assertEqual(generate.column_id("color-22"), "color-22")
        self.assertEqual(generate.column_id("color-129"), "color-129")
        self.assertEqual(generate.column_id("blue-1"), "blue")
        self.assertEqual(generate.column_id("blue+1"), "blue")


class DefaultFaceAdapter(unittest.TestCase):
    def setUp(self):
        self.defaults = DefaultFaces({
            "faces": {
                "sample": {
                    "chosenGuiLight": {
                        "foreground": "gray20",
                        "foregroundHex": "#333333",
                        "background": "white",
                        "backgroundHex": "#ffffff",
                        "weight": "bold",
                        "slant": "italic",
                        "underline": True,
                        "inherit": "parent",
                        "box": [":line-width", ["cons", 2, 2], ":style", "released-button"],
                    },
                    "effectiveGuiLight": {"foreground": "black", "foregroundHex": "#000000"},
                },
                "boxed": {
                    "chosenGuiLight": {
                        "box": [":line-width", -3, ":color", "gray20"],
                    },
                    "effectiveGuiLight": {},
                },
            }
        })

    def test_seed_uses_own_face_attributes_and_converts_boxes(self):
        self.assertEqual(self.defaults.seed("sample", effective=False), {
            "fg": "#333333",
            "bg": "#ffffff",
            "bold": True,
            "italic": True,
            "underline": True,
            "inherit": "parent",
            "box": {"style": "released", "width": 2, "color": None},
        })

    def test_color_reads_effective_hex_by_default(self):
        self.assertEqual(self.defaults.color("sample"), "#000000")

    def test_line_box_keeps_width_and_resolves_named_color(self):
        self.assertEqual(self.defaults.seed("boxed")["box"], {
            "style": "line",
            "width": 3,
            "color": "#333333",
        })

    def test_label_uses_captured_color_name_when_present(self):
        self.assertEqual(self.defaults.label("#333333", "fallback"), "gray20")

    def test_missing_snapshot_is_safe(self):
        defaults = DefaultFaces(None)
        self.assertFalse(defaults.available)
        self.assertEqual(defaults.face("missing"), {})
        self.assertEqual(defaults.seed("missing"), {})
        self.assertEqual(defaults.label("#000000", "fallback"), "fallback")

    def test_summary_reports_default_drift_fields(self):
        defaults = DefaultFaces({
            "meta": {"emacs-version": "30.2", "package-unresolved-face-count": 2},
            "ui-faces": ["sample"],
            "package-inventory": {"pkg": ["pkg-face"]},
            "faces": {
                "default": {
                    "effectiveGuiLight": {
                        "foregroundHex": "#000000",
                        "backgroundHex": "#ffffff",
                    },
                    "chosenGuiLight": {},
                },
                "sample": {
                    "chosenGuiLight": {"backgroundHex": "#ffffff"},
                    "effectiveGuiLight": {},
                },
                "pkg-face": {
                    "chosenGuiLight": {"inherit": "base-face"},
                    "effectiveGuiLight": {},
                },
            },
        })
        self.assertEqual(defaults.summary(), {
            "emacsVersion": "30.2",
            "default": {"foreground": "#000000", "background": "#ffffff"},
            "faceCount": 3,
            "packageFaceCount": 1,
            "packageUnresolvedFaceCount": 2,
            "uiOwnSeeds": {"sample": {"bg": "#ffffff"}},
            "packageInherits": {"pkg-face": "base-face"},
        })

    def test_changed_summary_reports_only_changed_top_level_keys(self):
        self.assertEqual(changed_summary({"a": 1, "b": 2}, {"a": 1, "b": 3, "c": 4}), {
            "b": {"before": 2, "after": 3},
            "c": {"before": None, "after": 4},
        })


class PackageFaceCoverage(unittest.TestCase):
    ALLOWED_DUPLICATES = {
        "magit-left-margin": ["magit", "magit-section"],
        "magit-section-child-count": ["magit", "magit-section"],
        "magit-section-heading": ["magit", "magit-section"],
        "magit-section-heading-selection": ["magit", "magit-section"],
        "magit-section-highlight": ["magit", "magit-section"],
        "magit-section-secondary-heading": ["magit", "magit-section"],
    }

    def app_faces(self):
        rows = []
        for app, data in generate.APPS.items():
            for face, _label, _seed in data["faces"]:
                rows.append((face, app))
        return rows

    def inventory_faces(self):
        inventory = generate.DEFAULTS.data.get("package-inventory", {})
        return {face for faces in inventory.values() for face in faces}

    def test_every_inventory_face_has_a_theme_studio_row(self):
        app_face_names = {face for face, _app in self.app_faces()}
        self.assertEqual(sorted(self.inventory_faces() - app_face_names), [])

    def test_duplicate_rows_are_intentional(self):
        counts = Counter(face for face, _app in self.app_faces())
        actual = defaultdict(list)
        for face, app in self.app_faces():
            if counts[face] > 1:
                actual[face].append(app)
        self.assertEqual(dict(sorted(actual.items())), self.ALLOWED_DUPLICATES)


class GeneratedDefaults(unittest.TestCase):
    def package_seed(self, app, face):
        for row_face, _label, seed in generate.APPS[app]["faces"]:
            if row_face == face:
                return seed
        self.fail(f"{app}/{face} is not in generated APPS")

    def test_ground_defaults_match_emacs_q_light_default(self):
        self.assertEqual(generate.MAP["bg"], "#ffffff")
        self.assertEqual(generate.MAP["p"], "#000000")
        self.assertEqual(generate.PALETTE[:2], [["#ffffff", "bg", "ground"], ["#000000", "fg", "ground"]])

    def test_ui_defaults_preserve_own_overlay_and_modeline_specs(self):
        self.assertEqual(generate.UIMAP["lazy-highlight"]["bg"], "#afeeee")
        self.assertIsNone(generate.UIMAP["lazy-highlight"]["fg"])
        self.assertEqual(generate.UIMAP["mode-line"]["box"], {"style": "released", "width": 1, "color": None})
        self.assertEqual(generate.UIMAP["mode-line-inactive"]["inherit"], "mode-line")
        self.assertEqual(generate.UIMAP["mode-line-inactive"]["box"], {"style": "line", "width": 1, "color": "#bfbfbf"})

    def test_representative_package_inherits_are_selected(self):
        self.assertEqual(self.package_seed("elfeed", "elfeed-search-filter-face")["inherit"], "mode-line-buffer-id")
        self.assertEqual(self.package_seed("ghostel", "ghostel-default")["inherit"], "default")

    def test_syntax_defaults_capture_font_lock_styles(self):
        self.assertEqual(generate.MAP["kw"], "#d3d3d3")
        self.assertTrue(generate.BOLD["kw"])
        self.assertFalse(generate.ITALIC_MAP["kw"])
        self.assertEqual(generate.MAP["str"], "#696969")
        self.assertFalse(generate.BOLD["str"])
        self.assertTrue(generate.ITALIC_MAP["str"])


if __name__ == "__main__":
    unittest.main()
