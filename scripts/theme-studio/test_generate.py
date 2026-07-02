"""Tests for the theme-studio page generator (generate.py).

The generator's risky logic is the export-strip and the placeholder substitution
that inline colormath.js and the sample/palette data into the page. A bug there
ships a broken theme-studio.html that the JS unit tests can't see. These tests
exercise the strip in isolation and assert the assembled page has every
placeholder filled and carries the colormath body verbatim.

Run: python3 -m unittest test_generate   (from scripts/theme-studio/)
"""
import os
import io
import json
import tempfile
import runpy
import unittest
from collections import Counter, defaultdict
from contextlib import redirect_stdout

import generate  # importable without side effects: the file write is __main__-guarded
import face_coverage
from unittest import mock


class ClassifyBucket(unittest.TestCase):
    """Characterization of face_coverage.classify's core/general/package decision,
    locking each branch before the named-locals rewrite. bucket_of_source is mocked
    to identity, so the src dict maps each face straight to its bucket name."""

    def _classify(self, src, pkgfaces=(), name="x"):
        with mock.patch.object(face_coverage, "bucket_of_source", lambda s: s):
            return face_coverage.classify(name, list(src), src, set(pkgfaces))

    def test_emacs_core_short_circuits_to_core(self):
        self.assertEqual(face_coverage.classify("emacs-core", [], {}, set()), "core")

    def test_nothing_loaded_with_a_package_face_is_package(self):
        self.assertEqual(self._classify({"a": "unloaded", "b": "unloaded"}, pkgfaces={"b"}), "package")

    def test_nothing_loaded_without_a_package_face_is_general(self):
        self.assertEqual(self._classify({"a": "unloaded"}), "general")

    def test_elpa_plurality_is_package(self):
        self.assertEqual(self._classify({"a": "elpa", "b": "elpa", "c": "builtin"}), "package")

    def test_elpa_tied_with_builtin_is_package(self):
        self.assertEqual(self._classify({"a": "elpa", "b": "builtin"}), "package")

    def test_other_beats_builtin_and_ties_elpa_is_package(self):
        self.assertEqual(self._classify({"a": "other", "b": "other", "c": "elpa", "d": "builtin"}), "package")

    def test_builtin_plurality_is_general(self):
        self.assertEqual(self._classify({"a": "builtin", "b": "builtin", "c": "elpa"}), "general")
from app_inventory import face_rows
from default_faces import DefaultFaces, changed_summary
from face_specs import face_spec, package_face_spec, ui_face_spec


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
        with open(os.path.join(generate.HERE, "colormath.js")) as src:
            self.cm_src = src.read()

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
        "PALETTE_GENERATOR_CORE_J", "PALETTE_GENERATOR_UI_J",
        "PALETTE_ACTIONS_J", "BROWSER_GATES_J",
        "COLORMATH_J", "SAMPLES_J", "PALETTE_J", "CATS_J",
        "UIFACES_J", "UIMAP_J", "APPS_J", "SYNTAX_J", "MAP_J",
        "COLOR_NAMES_J", "FACE_DOCS_J", "SYNTAX_DOCS_J",
    ]

    def test_every_placeholder_is_substituted(self):
        for token in self.PLACEHOLDERS:
            self.assertNotIn(token, generate.HTML, f"{token} left unsubstituted")

    def test_face_docs_maps_embed_a_known_docstring(self):
        # The face/syntax docstring maps inline so element hovers can show them.
        # default is always present; its first line is stable across Emacs builds.
        self.assertIn("Basic default face.", generate.FACE_DOCS["default"])
        self.assertIn(json.dumps(generate.FACE_DOCS), generate.HTML)

    def test_syntax_docs_resolve_categories_to_face_docstrings(self):
        # The syntax table is keyed by category (kw, doc, ...); each resolves to
        # its font-lock face's docstring via build-theme's canonical map.
        self.assertIn("keyword", generate.SYNTAX_DOCS["kw"].lower())
        self.assertIn(json.dumps(generate.SYNTAX_DOCS), generate.HTML)

    def test_page_carries_each_inlined_body_verbatim(self):
        # Python-side inline-integrity: every verbatim-inlined module (no data
        # placeholders, exports/imports stripped) must appear in the page byte for
        # byte, so the inlined copy and the unit-tested module cannot drift. Checked
        # at build time rather than after a round-trip. app-util.js's import line is
        # already stripped in APP_UTIL_BODY.
        for name in ("COLORMATH_BODY", "APP_CORE_BODY", "APP_UTIL_BODY",
                     "PALETTE_GENERATOR_CORE_BODY", "PALETTE_GENERATOR_UI_BODY",
                     "PALETTE_ACTIONS_BODY", "BROWSER_GATES_BODY"):
            with self.subTest(body=name):
                self.assertIn(getattr(generate, name), generate.HTML)

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


class LanguageSamples(unittest.TestCase):
    def _tokens(self, lang):
        return [tok for line in generate.SAMPLES[lang] for tok in line]

    def test_rust_and_zig_are_available_in_the_language_selector(self):
        self.assertIn("Rust", generate.SAMPLES)
        self.assertIn("Zig", generate.SAMPLES)

    def test_rust_sample_exercises_language_specific_categories(self):
        tokens = self._tokens("Rust")
        cats = {k for k, _ in tokens}
        text = "".join(t for _, t in tokens)
        for cat in ("dec", "ty", "bi", "kw", "op", "var"):
            self.assertIn(cat, cats)
        self.assertIn("'a", text)
        self.assertIn("trait", text)
        self.assertIn("vec!", text)

    def test_zig_sample_exercises_language_specific_categories(self):
        tokens = self._tokens("Zig")
        cats = {k for k, _ in tokens}
        text = "".join(t for _, t in tokens)
        for cat in ("bi", "kw", "ty", "con", "op", "prop"):
            self.assertIn(cat, cats)
        self.assertIn("comptime", text)
        self.assertIn("@import", text)
        self.assertIn("error.MissingColor", text)

    def test_expanded_language_set_is_registered_and_renders(self):
        # Every added language is selectable and renders a non-trivial sample that
        # exercises keywords and carries a comment.
        added = ["Racket", "Scheme", "Haskell", "OCaml", "Scala", "Kotlin",
                 "Swift", "Lua", "Ruby", "Perl", "R", "Erlang", "SQL", "PHP",
                 "Ada", "Fortran", "MATLAB", "Assembly"]
        for lang in added:
            self.assertIn(lang, generate.SAMPLES, f"{lang} not in the language selector")
            tokens = self._tokens(lang)
            cats = {k for k, _ in tokens}
            self.assertGreater(len(tokens), 40, f"{lang} sample is too short")
            self.assertIn("kw", cats, f"{lang} sample has no keywords")
            self.assertIn("cmd", cats, f"{lang} sample has no comment")


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
        # The legacy "bold" migrates to weight "bold" through face_spec.
        self.assertEqual(ui_face_spec({"bg": "#ffffff", "bold": True}), {
            "fg": None,
            "bg": "#ffffff",
            "distant-fg": None,
            "family": None,
            "weight": "bold",
            "slant": None,
            "underline": None,
            "strike": None,
            "overline": None,
            "box": None,
            "inverse": False,
            "extend": False,
            "inherit": None,
            "height": None,
        })

    def test_ui_face_spec_carries_inherit_and_height(self):
        # inherit/height are no longer package-only; a ui face can set them.
        spec = ui_face_spec({"inherit": "shadow", "height": 1.3})
        self.assertEqual(spec["inherit"], "shadow")
        self.assertEqual(spec["height"], 1.3)

    def test_face_spec_migrates_legacy_style_booleans(self):
        spec = ui_face_spec({"italic": True, "underline": True, "strike": True})
        self.assertEqual(spec["slant"], "italic")
        self.assertEqual(spec["underline"], {"style": "line", "color": None})
        self.assertEqual(spec["strike"], {"color": None})
        self.assertNotIn("bold", spec)
        self.assertNotIn("italic", spec)

    def test_package_face_spec_fills_structure_fields(self):
        self.assertEqual(package_face_spec({"inherit": "base", "height": 1.2}), {
            "fg": None,
            "bg": None,
            "distant-fg": None,
            "family": None,
            "weight": None,
            "slant": None,
            "underline": None,
            "strike": None,
            "overline": None,
            "box": None,
            "inverse": False,
            "extend": False,
            "inherit": "base",
            "height": 1.2,
        })

    def test_generated_color_names_are_base_columns_when_legacy(self):
        self.assertEqual(generate.column_id("color-22"), "color-22")
        self.assertEqual(generate.column_id("color-129"), "color-129")
        self.assertEqual(generate.column_id("blue-1"), "blue")
        self.assertEqual(generate.column_id("blue+1"), "blue")
        self.assertEqual(generate.column_id("blue1"), "blue")
        self.assertEqual(generate.column_id("grey80"), "grey")
        self.assertEqual(generate.column_id("orchid3"), "orchid")


class GeneratorStateHelpers(unittest.TestCase):
    def test_initial_maps_use_column_fallbacks_without_defaults_snapshot(self):
        cols = {"kw": [None, True], "str": [None, False]}
        color_map, bold, italic = generate.initial_maps(cols, DefaultFaces(None))
        self.assertEqual(color_map["bg"], "#000000")
        self.assertEqual(color_map["p"], "#ffffff")
        self.assertTrue(bold["kw"])
        self.assertFalse(bold["str"])
        self.assertFalse(italic["kw"])

    def test_build_uimap_uses_fallback_styles_without_defaults_snapshot(self):
        uimap = generate.build_uimap(generate.UI_FACES, DefaultFaces(None))
        self.assertTrue(uimap["link"]["underline"])
        self.assertEqual(uimap["mode-line"]["box"], {"style": "released", "width": 1, "color": None})

    def test_mode_line_highlight_defaults_to_raised_box(self):
        # The face is absent from the snapshot, so it must get the raised box in
        # both the with-snapshot and no-snapshot branches.
        raised = {"style": "released", "width": 1, "color": None}
        self.assertEqual(generate.UIMAP["mode-line-highlight"]["box"], raised)
        no_snapshot = generate.build_uimap(generate.UI_FACES, DefaultFaces(None))
        self.assertEqual(no_snapshot["mode-line-highlight"]["box"], raised)

    def test_hover_box_default_yields_to_existing_box(self):
        uimap = {"mode-line-highlight": ui_face_spec({"box": {"style": "line", "width": 2, "color": "#abcdef"}})}
        generate.apply_hover_box_default(uimap)
        self.assertEqual(uimap["mode-line-highlight"]["box"], {"style": "line", "width": 2, "color": "#abcdef"})

    def test_build_syntax_uses_map_and_style_fallbacks_without_defaults_snapshot(self):
        syntax = generate.build_syntax(
            {"kw": [None, True]},
            {"kw": "#d3d3d3"},
            {"kw": True},
            {"kw": False},
            DefaultFaces(None),
        )
        self.assertEqual(syntax["kw"]["fg"], "#d3d3d3")
        self.assertEqual(syntax["kw"]["weight"], "bold")
        self.assertIsNone(syntax["kw"]["slant"])

    def test_builtin_fallback_styles_fill_known_emacs_styles(self):
        uimap = {
            name: ui_face_spec()
            for name in (
                "link", "lazy-highlight", "show-paren-match",
                "error", "warning", "success",
                "mode-line", "mode-line-inactive",
            )
        }
        generate.apply_builtin_fallback_styles(uimap)
        line_underline = {"style": "line", "color": None}
        self.assertEqual(uimap["link"]["underline"], line_underline)
        self.assertEqual(uimap["lazy-highlight"]["underline"], line_underline)
        self.assertEqual(uimap["show-paren-match"]["underline"], line_underline)
        self.assertEqual(uimap["error"]["weight"], "bold")
        self.assertEqual(uimap["warning"]["weight"], "bold")
        self.assertEqual(uimap["success"]["weight"], "bold")
        self.assertEqual(uimap["mode-line"]["box"], {"style": "released", "width": 1, "color": None})
        self.assertEqual(uimap["mode-line-inactive"]["box"], {"style": "released", "width": 1, "color": None})

    def test_seed_basics_replace_palette_ui_and_locks(self):
        palette = [["#ffffff", "bg", "ground"]]
        uimap = {"region": ui_face_spec()}
        data = {
            "palette": [["#101010", "bg", "ground"], ["#eeeeee", "fg", "ground"]],
            "ui": {"region": ui_face_spec({"bg": "#67809c"})},
            "locks": ["region"],
        }
        seeded_palette, seeded_ui, locks = generate.apply_seed_basics(data, palette, uimap, [])
        self.assertEqual(seeded_palette, data["palette"])
        self.assertEqual(seeded_ui["region"]["bg"], "#67809c")
        self.assertEqual(locks, ["region"])

    def test_load_seed_data_reads_relative_json_seed(self):
        with tempfile.NamedTemporaryFile("w", dir=generate.HERE, suffix=".json", delete=False) as tmp:
            tmp.write('{"locks":["kw"]}')
            name = os.path.basename(tmp.name)
        try:
            self.assertEqual(generate.load_seed_data(name), {"locks": ["kw"]})
        finally:
            os.unlink(tmp.name)

    def test_syntax_seed_updates_known_roles_and_map(self):
        syntax = {"kw": face_spec(), "str": face_spec({"fg": "#111111"})}
        color_map = {"kw": "", "str": "#111111"}
        generate.apply_syntax_seed({
            "syntax": {
                "kw": {"fg": "#222222", "bold": True},
                "unknown": {"fg": "#ff0000"},
            }
        }, syntax, color_map)
        self.assertEqual(syntax["kw"]["fg"], "#222222")
        self.assertEqual(syntax["kw"]["weight"], "bold")
        self.assertEqual(color_map["kw"], "#222222")
        self.assertNotIn("unknown", syntax)

    def test_seed_package_overrides_replace_known_package_faces(self):
        apps = {"demo": {"faces": [["demo-face", "face", {"fg": "#111111"}]]}}
        generate.apply_seed_packages(apps, {
            "packages": {"demo": {"demo-face": {"fg": "#222222", "source": "user"}}}
        }, "seed.json")
        self.assertEqual(apps["demo"]["faces"][0][2]["fg"], "#222222")
        self.assertEqual(apps["demo"]["faces"][0][2]["source"], "user")

    def test_palette_color_names_are_unique_and_stable_columns(self):
        palette = [["#111111", "blue", "blue"]]
        defaults = DefaultFaces(None)
        generate.add_palette_color(palette, defaults, "#222222", "blue")
        self.assertEqual(palette[-1], ["#222222", "blue-2", "blue"])

    def test_default_palette_collection_includes_box_colors(self):
        palette = [["#000000", "bg", "ground"], ["#ffffff", "fg", "ground"]]
        defaults = DefaultFaces(None)
        generate.add_default_palette_colors(
            palette,
            {"bg": "#000000", "p": "#ffffff", "kw": "#111111"},
            {"kw": face_spec({"fg": "#111111", "bg": "#222222", "box": {"color": "#333333"}})},
            {"region": ui_face_spec({"fg": "#444444", "bg": "#555555", "box": {"color": "#666666"}})},
            {"demo": {"faces": [["demo-face", "face", {"fg": "#777777", "bg": "#888888", "box": {"color": "#999999"}}]]}},
            defaults,
        )
        colors = {entry[0] for entry in palette}
        for color in ("#333333", "#666666", "#999999"):
            self.assertIn(color, colors)

    def test_render_theme_studio_writes_html_to_requested_path(self):
        with tempfile.NamedTemporaryFile(delete=False) as tmp:
            path = tmp.name
        try:
            out = io.StringIO()
            with redirect_stdout(out):
                generate.render_theme_studio(path)
            self.assertIn("wrote " + path, out.getvalue())
            with open(path) as generated:
                self.assertIn("<!doctype html>", generated.read().lower())
        finally:
            os.unlink(path)

    def test_generate_script_entrypoint_writes_default_output(self):
        out = io.StringIO()
        with redirect_stdout(out):
            runpy.run_path(generate.__file__, run_name="__main__")
        self.assertIn("wrote " + generate.OUT, out.getvalue())


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
                "rich": {
                    "chosenGuiLight": {
                        "distantForeground": "black",
                        "distantForegroundHex": "#000000",
                        "overline": "t",
                        "inverseVideo": "t",
                        "extend": "t",
                    },
                    "effectiveGuiLight": {},
                },
            }
        })

    def test_seed_uses_own_face_attributes_and_converts_boxes(self):
        self.assertEqual(self.defaults.seed("sample", effective=False), {
            "fg": "#333333",
            "bg": "#ffffff",
            "weight": "bold",
            "slant": "italic",
            "underline": {"style": "line", "color": None},
            "inherit": "parent",
            "box": {"style": "released", "width": 2, "color": None},
        })

    def test_seed_emits_the_additive_attrs_when_the_snapshot_has_them(self):
        self.assertEqual(self.defaults.seed("rich", effective=False), {
            "distant-fg": "#000000",
            "overline": {"color": None},
            "inverse": True,
            "extend": True,
        })

    def test_seed_omits_additive_attrs_when_the_snapshot_lacks_them(self):
        seeded = self.defaults.seed("sample", effective=False)
        for key in ("distant-fg", "overline", "inverse", "extend"):
            self.assertNotIn(key, seeded)

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

    def test_syntax_defaults_capture_font_lock_styles(self):
        self.assertEqual(generate.MAP["kw"], "#d3d3d3")
        self.assertEqual(generate.SYNTAX["kw"]["weight"], "bold")
        self.assertIsNone(generate.SYNTAX["kw"]["slant"])
        self.assertEqual(generate.MAP["str"], "#696969")
        self.assertIsNone(generate.SYNTAX["str"]["weight"])
        self.assertEqual(generate.SYNTAX["str"]["slant"], "italic")


class NerdIconsLegend(unittest.TestCase):
    """The committed nerd-icons-legend.json artifact and the loader fallback."""

    def _write(self, content):
        path = os.path.join(tempfile.mkdtemp(), "nerd-icons-legend.json")
        with open(path, "w") as out:
            out.write(content)
        return path

    def test_committed_artifact_has_valid_rows(self):
        rows = generate.load_nerd_icons_legend()
        self.assertIsNotNone(rows, "committed nerd-icons-legend.json should load")
        self.assertTrue(rows)
        for row in rows:
            for field in generate.NERD_ICONS_LEGEND_FIELDS:
                self.assertIsInstance(row.get(field), str)
                self.assertTrue(row[field])
            self.assertTrue(row["face"].startswith("nerd-icons-"))
            self.assertIn(row["category"], ("extension", "dir", "command", "buffer"))

    def test_absent_artifact_falls_back_to_none(self):
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_legend("/no/such/legend.json"))
        self.assertIn("absent", out.getvalue())

    def test_malformed_artifact_falls_back_to_none(self):
        path = self._write("{not json")
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_legend(path))
        self.assertIn("malformed", out.getvalue())

    def test_empty_artifact_falls_back_to_none(self):
        path = self._write("[]")
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_legend(path))
        self.assertIn("empty", out.getvalue())

    def test_row_missing_a_field_falls_back_to_none(self):
        path = self._write(json.dumps([{"key": "ext:el", "label": "init.el",
                                        "face": "nerd-icons-purple", "category": "extension"}]))
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_legend(path))
        self.assertIn("invalid", out.getvalue())

    def test_nerd_icons_registered_as_bespoke_legend_app(self):
        app = generate.APPS.get("nerd-icons")
        self.assertIsNotNone(app, "nerd-icons should be a bespoke app with the legend present")
        self.assertEqual(app["preview"], "nerdicons")
        self.assertTrue(app.get("legend"))
        self.assertGreaterEqual(len(app["faces"]), 30)
        # The dir-completion face is a different package and keeps its own app.
        self.assertIn("nerd-icons-completion", generate.APPS)

    def test_nerd_icons_app_faces_are_seeded_with_native_colors(self):
        # apply_default_face_seeds fills the editable rows from emacs-default-faces.json.
        rows = {r[0]: r[2] for r in generate.APPS["nerd-icons"]["faces"]}
        self.assertIn("nerd-icons-blue", rows)
        self.assertTrue(rows["nerd-icons-blue"], "nerd-icons-blue should carry a native seed")

    def test_legend_loads_from_object_shaped_artifact(self):
        # The committed artifact is now an object {legend, gallery}; the legend
        # loader must read the "legend" key, not assume a bare array.
        path = self._write(json.dumps({"legend": [
            {"key": "ext:el", "label": "init.el", "face": "nerd-icons-purple",
             "category": "extension", "glyph": "x"}], "gallery": []}))
        rows = generate.load_nerd_icons_legend(path)
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["face"], "nerd-icons-purple")


class NerdIconsGallery(unittest.TestCase):
    """The committed gallery (full colored catalog) and its loader fallback."""

    def _write(self, content):
        path = os.path.join(tempfile.mkdtemp(), "nerd-icons-legend.json")
        with open(path, "w") as out:
            out.write(content)
        return path

    def test_committed_artifact_has_valid_groups(self):
        groups = generate.load_nerd_icons_gallery()
        self.assertIsNotNone(groups, "committed gallery should load")
        self.assertTrue(groups)
        for g in groups:
            self.assertTrue(g["face"].startswith("nerd-icons-"))
            self.assertIsInstance(g["hue"], (int, float))
            self.assertTrue(g["glyphs"])
            for e in g["glyphs"]:
                for field in generate.NERD_ICONS_GALLERY_GLYPH_FIELDS:
                    self.assertIsInstance(e.get(field), str)
                    self.assertTrue(e[field])

    def test_groups_are_ordered_by_hue(self):
        groups = generate.load_nerd_icons_gallery()
        hues = [g["hue"] for g in groups]
        self.assertEqual(hues, sorted(hues), "color rows cluster by hue (ascending)")

    def test_icons_are_deduplicated_within_a_group(self):
        for g in generate.load_nerd_icons_gallery():
            names = [e["name"] for e in g["glyphs"]]
            self.assertEqual(len(names), len(set(names)), f"{g['face']} repeats an icon name")

    def test_absent_artifact_falls_back_to_none(self):
        with redirect_stdout(io.StringIO()):
            self.assertIsNone(generate.load_nerd_icons_gallery("/no/such/legend.json"))

    def test_malformed_artifact_falls_back_to_none(self):
        path = self._write("{not json")
        with redirect_stdout(io.StringIO()):
            self.assertIsNone(generate.load_nerd_icons_gallery(path))

    def test_legacy_array_only_artifact_has_no_gallery(self):
        # A v1-era bare-array file carries a legend but no gallery -> None, no crash.
        path = self._write(json.dumps([{"key": "ext:el"}]))
        with redirect_stdout(io.StringIO()):
            self.assertIsNone(generate.load_nerd_icons_gallery(path))

    def test_group_missing_a_field_falls_back_to_none(self):
        # Missing hue and glyphs -> invalid.
        path = self._write(json.dumps({"legend": [], "gallery": [{"face": "nerd-icons-blue"}]}))
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_gallery(path))
        self.assertIn("invalid", out.getvalue())

    def test_glyph_entry_missing_a_field_falls_back_to_none(self):
        path = self._write(json.dumps({"gallery": [
            {"face": "nerd-icons-blue", "hue": 212, "glyphs": [{"glyph": "x"}]}]}))
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_gallery(path))
        self.assertIn("invalid", out.getvalue())

    def test_group_with_empty_glyphs_falls_back_to_none(self):
        path = self._write(json.dumps({"gallery": [
            {"face": "nerd-icons-blue", "hue": 212, "glyphs": []}]}))
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_gallery(path))
        self.assertIn("invalid", out.getvalue())

    def test_group_with_a_foreign_face_falls_back_to_none(self):
        path = self._write(json.dumps({"gallery": [
            {"face": "rainbow-delimiters-depth-1", "hue": 212,
             "glyphs": [{"glyph": "x", "name": "nf-x"}]}]}))
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_gallery(path))
        self.assertIn("invalid", out.getvalue())

    def test_group_with_a_non_numeric_hue_falls_back_to_none(self):
        path = self._write(json.dumps({"gallery": [
            {"face": "nerd-icons-blue", "hue": "212",
             "glyphs": [{"glyph": "x", "name": "nf-x"}]}]}))
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_gallery(path))
        self.assertIn("invalid", out.getvalue())

    def test_non_dict_glyph_entry_falls_back_to_none(self):
        path = self._write(json.dumps({"gallery": [
            {"face": "nerd-icons-blue", "hue": 212, "glyphs": ["not-a-dict"]}]}))
        with redirect_stdout(io.StringIO()) as out:
            self.assertIsNone(generate.load_nerd_icons_gallery(path))
        self.assertIn("invalid", out.getvalue())

    def test_nerd_icons_app_carries_the_gallery(self):
        app = generate.APPS.get("nerd-icons")
        self.assertIsNotNone(app)
        self.assertTrue(app.get("gallery"), "nerd-icons app should carry the gallery groups")
        faces = {g["face"] for g in app["gallery"]}
        self.assertIn("nerd-icons-blue", faces)


class PinnedPackages(unittest.TestCase):
    """The ecosystem coverage policy's mechanism: PINNED_PACKAGE_FACES is the
    curated record of packages retired from this config. A pinned package is
    always marked not loaded (pinning happens exactly at retirement), and it
    survives inventory regeneration even when uninstalled. Its face list stays
    fresh from the live inventory when present (a still-installed dependency
    may grow faces); the pin is the fallback when the inventory drops it."""

    def _apps_with_inventory(self, inventory):
        import app_inventory
        with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False) as f:
            json.dump(inventory, f)
            path = f.name
        try:
            return app_inventory.add_inventory_apps({}, path)
        finally:
            os.unlink(path)

    def test_pinned_package_absent_from_inventory_is_added_unloaded(self):
        apps = self._apps_with_inventory({"someother": ["someother-face"]})
        self.assertIn("ghostel", apps)
        self.assertTrue(apps["ghostel"]["unloaded"])
        self.assertIn("not loaded", apps["ghostel"]["label"])
        faces = {row[0] for row in apps["ghostel"]["faces"]}
        self.assertIn("ghostel-default", faces)

    def test_pinned_package_in_inventory_keeps_live_faces_still_flagged (self):
        apps = self._apps_with_inventory({"ghostel": ["ghostel-default", "ghostel-brand-new"]})
        self.assertTrue(apps["ghostel"]["unloaded"])
        self.assertIn("not loaded", apps["ghostel"]["label"])
        faces = {row[0] for row in apps["ghostel"]["faces"]}
        self.assertIn("ghostel-brand-new", faces)

    def test_unpinned_inventory_package_is_not_flagged(self):
        apps = self._apps_with_inventory({"someother": ["someother-face"]})
        self.assertFalse(apps["someother"].get("unloaded"))
        self.assertNotIn("not loaded", apps["someother"]["label"])

    def test_pinned_apps_carry_an_explanatory_hover(self):
        apps = self._apps_with_inventory({})
        self.assertIn("pinned", apps["ghostel"].get("hover", ""))

    def test_all_the_icons_is_pinned(self):
        apps = self._apps_with_inventory({})
        self.assertIn("all-the-icons", apps)
        self.assertTrue(apps["all-the-icons"]["unloaded"])
        faces = {row[0] for row in apps["all-the-icons"]["faces"]}
        self.assertIn("all-the-icons-blue", faces)


class BespokePreviewFaceCoverage(unittest.TestCase):
    """Realism gate for the bespoke scenes built over inventory apps: a scene
    that skips faces silently reads as "themed everything" when it didn't.
    Every face of each app listed here must appear verbatim in previews.js."""

    FULL_COVERAGE_APPS = [
        "company", "company-box", "transient", "magit-section",
        "rainbow-delimiters", "web-mode",
        "vertico", "marginalia", "consult", "embark", "orderless",
    ]

    def test_every_face_appears_in_the_renderer(self):
        with open(os.path.join(os.path.dirname(__file__), "previews.js")) as f:
            body = f.read()
        missing = []
        for app in self.FULL_COVERAGE_APPS:
            for face, _label, _seed in generate.APPS[app]["faces"]:
                # Renderers build names as prefix+suffix (os(a,'company-'+f,..)),
                # so accept the bare suffix appearing after the app prefix too.
                suffix = face
                for prefix in (app + "-", "magit-", "rainbow-delimiters-"):
                    if face.startswith(prefix):
                        suffix = face[len(prefix):]
                        break
                if face not in body and suffix not in body:
                    missing.append(face)
        self.assertEqual(missing, [])


if __name__ == "__main__":
    unittest.main()
