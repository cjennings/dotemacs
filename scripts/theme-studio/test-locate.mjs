// Unit tests for the preview-locate pure helpers (app-core.js): the owner-qualified
// face registry and the lookup / title / validation / on-pane helpers that previews
// read. Pure data only -- no DOM, no globals, no HTML. The stateful previewSpan
// adapter lives in previews.js and is browser-gated, not tested here.
//
// Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import {
  buildLocateRegistry, locateFaceMeta, formatLocateTitle, previewFaceAttrs, isLocateOnPane,
} from './app-core.js';

// A constructed model: two package apps that BOTH own a face literally named
// 'org-todo' (the cross-owner name collision finding 7 guards against), plus the
// UI surface owning minibuffer-prompt. PKGMAP/UIMAP store resolved hex the way the
// live maps do; MAP carries the ground floors effFg/effBg fall back to.
const MAP = { p: '#d6dae0', bg: '#101014' };
const APPS = {
  'org-faces': { label: 'org-faces', faces: [['org-todo', 'TODO', { fg: 'red' }]] },
  'org-mode':  { label: 'org-mode',  faces: [['org-todo', 'TODO', { fg: 'blue' }]] },
};
const PKGMAP = {
  'org-faces': { 'org-todo': { fg: '#cc3333', bg: null, inherit: null, source: 'user' } },
  'org-mode':  { 'org-todo': { fg: '#3344cc', bg: null, inherit: null, source: 'user' } },
};
const UIMAP = {
  'minibuffer-prompt': { fg: '#899bb1', bg: null, inherit: null, source: 'user' },
};

test('buildLocateRegistry: Normal — covers package and UI faces with their effective fg', () => {
  const reg = buildLocateRegistry(APPS, PKGMAP, UIMAP, MAP);
  const pkg = locateFaceMeta('org-faces', 'org-todo', reg);
  assert.equal(pkg.surface, 'package');
  assert.equal(pkg.owner, 'org-faces');
  assert.equal(pkg.section, 'org-faces');
  assert.equal(pkg.value.fg, '#cc3333');

  const ui = locateFaceMeta('@ui', 'minibuffer-prompt', reg);
  assert.equal(ui.surface, 'ui');
  assert.equal(ui.owner, '@ui');
  assert.equal(ui.value.fg, '#899bb1');
});

test('buildLocateRegistry: Boundary — same face name under two owners stays distinct', () => {
  const reg = buildLocateRegistry(APPS, PKGMAP, UIMAP, MAP);
  const a = locateFaceMeta('org-faces', 'org-todo', reg);
  const b = locateFaceMeta('org-mode', 'org-todo', reg);
  assert.notEqual(a, b);
  assert.equal(a.value.fg, '#cc3333');
  assert.equal(b.value.fg, '#3344cc');
});

test('locateFaceMeta: Error — an unknown owner/face is unassigned, not a collision', () => {
  const reg = buildLocateRegistry(APPS, PKGMAP, UIMAP, MAP);
  const miss = locateFaceMeta('org-faces', 'no-such-face', reg);
  assert.equal(miss.unassigned, true);
});

test('isLocateOnPane: Normal — on-pane only when the owner is the viewed pane', () => {
  assert.equal(isLocateOnPane('org-faces', 'org-faces'), true);
  assert.equal(isLocateOnPane('org-mode', 'org-faces'), false);
  assert.equal(isLocateOnPane('@ui', '@ui'), true);
  assert.equal(isLocateOnPane('@ui', 'org-faces'), false);
});

// --- formatLocateTitle: one assertion per source state ----------------------

test('formatLocateTitle: Normal — direct fg only', () => {
  const reg = buildLocateRegistry(APPS, PKGMAP, UIMAP, MAP);
  const t = formatLocateTitle(locateFaceMeta('org-faces', 'org-todo', reg));
  assert.equal(t, 'org-faces, org-todo, fg #cc3333 (direct)');
});

test('formatLocateTitle: Normal — direct fg and bg', () => {
  const pkgmap = { app: { face: { fg: '#aabbcc', bg: '#223344', inherit: null, source: 'user' } } };
  const apps = { app: { label: 'App', faces: [['face', 'F', {}]] } };
  const reg = buildLocateRegistry(apps, pkgmap, {}, MAP);
  const t = formatLocateTitle(locateFaceMeta('app', 'face', reg));
  assert.equal(t, 'App, face, fg #aabbcc (direct), bg #223344 (direct)');
});

test('formatLocateTitle: Normal — inherited package fg names the source face', () => {
  const pkgmap = {
    app: {
      string: { fg: '#8fbf73', bg: null, inherit: null, source: 'user' },
      doc:    { fg: null, bg: null, inherit: 'string', source: 'user' },
    },
  };
  const apps = { app: { label: 'App', faces: [['string', 'S', {}], ['doc', 'D', {}]] } };
  const reg = buildLocateRegistry(apps, pkgmap, {}, MAP);
  const meta = locateFaceMeta('app', 'doc', reg);
  assert.equal(meta.value.fg, '#8fbf73');
  // The fg source note and the structural :inherit attribute are distinct facts —
  // a face can inherit yet set its own fg directly — so both appear.
  assert.equal(formatLocateTitle(meta), 'App, doc, fg #8fbf73 (inherited from string), inherit string');
});

test('formatLocateTitle: Normal — inherited UI fg via the built-in UI chain', () => {
  // mode-line-inactive inherits mode-line through UI_INHERIT; an unset
  // mode-line-inactive fg renders mode-line's fg, so the title must say so.
  const uimap = {
    'mode-line': { fg: '#202830', bg: null, inherit: null },
    'mode-line-inactive': { fg: null, bg: null, inherit: null },
  };
  const reg = buildLocateRegistry({}, {}, uimap, MAP);
  const meta = locateFaceMeta('@ui', 'mode-line-inactive', reg);
  assert.equal(meta.value.fg, '#202830');
  assert.equal(formatLocateTitle(meta), 'UI faces, mode-line-inactive, fg #202830 (inherited from mode-line)');
});

test('formatLocateTitle: Boundary — cleared fg shows the rendered default with a cleared note', () => {
  const pkgmap = { app: { face: { fg: null, bg: null, inherit: null, source: 'cleared' } } };
  const apps = { app: { label: 'App', faces: [['face', 'F', {}]] } };
  const reg = buildLocateRegistry(apps, pkgmap, {}, MAP);
  const meta = locateFaceMeta('app', 'face', reg);
  assert.equal(meta.value.fg, MAP.p, 'value is the rendered default, matching the pixels');
  // A fully-cleared face notes both attributes; the fg carries the rendered default hex.
  assert.equal(formatLocateTitle(meta), 'App, face, fg #d6dae0 (cleared, rendering as default), bg cleared, rendering as default');
});

test('formatLocateTitle: Boundary — cleared bg notes the cleared state without a phantom hex', () => {
  const pkgmap = { app: { face: { fg: '#ffffff', bg: null, inherit: null, source: 'cleared' } } };
  const apps = { app: { label: 'App', faces: [['face', 'F', {}]] } };
  const reg = buildLocateRegistry(apps, pkgmap, {}, MAP);
  const t = formatLocateTitle(locateFaceMeta('app', 'face', reg));
  // fg is set directly, so it reports 'direct'; only the null bg is cleared. The
  // source note is per attribute, not per face.
  assert.equal(t, 'App, face, fg #ffffff (direct), bg cleared, rendering as default');
});

test('formatLocateTitle: Normal — non-default structural attributes are listed', () => {
  const pkgmap = { app: { face: { fg: '#ffffff', bg: null, weight: 'bold', slant: 'italic', underline: { style: 'line', color: null }, inherit: null, source: 'user' } } };
  const apps = { app: { label: 'App', faces: [['face', 'F', {}]] } };
  const reg = buildLocateRegistry(apps, pkgmap, {}, MAP);
  const t = formatLocateTitle(locateFaceMeta('app', 'face', reg));
  assert.equal(t, 'App, face, fg #ffffff (direct), bold, italic, underline');
});

test('formatLocateTitle: Error — an unassigned meta reads "unassigned"', () => {
  const reg = buildLocateRegistry(APPS, PKGMAP, UIMAP, MAP);
  assert.equal(formatLocateTitle(locateFaceMeta('org-faces', 'ghost', reg)), 'ghost, unassigned');
});

// --- previewFaceAttrs: owner-aware validation -------------------------------

test('previewFaceAttrs: Normal — a known owner/face validates; a bad owner is rejected', () => {
  const reg = buildLocateRegistry(APPS, PKGMAP, UIMAP, MAP);
  assert.ok(previewFaceAttrs('org-faces', 'org-todo', reg), 'known face validates');
  assert.equal(previewFaceAttrs('org-mode', 'minibuffer-prompt', reg), null, 'a UI face under a package owner is rejected');
  assert.equal(previewFaceAttrs('nope', 'org-todo', reg), null, 'an unknown owner is rejected');
});

// --- lifecycle + perf -------------------------------------------------------

test('buildLocateRegistry: lifecycle — a rebuild after an edit reflects the new value', () => {
  const pkgmap = { app: { face: { fg: '#111111', bg: null, inherit: null, source: 'user' } } };
  const apps = { app: { label: 'App', faces: [['face', 'F', {}]] } };
  let reg = buildLocateRegistry(apps, pkgmap, {}, MAP);
  assert.equal(locateFaceMeta('app', 'face', reg).value.fg, '#111111');
  pkgmap.app.face.fg = '#222222';            // simulate an assignment edit
  reg = buildLocateRegistry(apps, pkgmap, {}, MAP);  // rebuild on the batch
  assert.equal(locateFaceMeta('app', 'face', reg).value.fg, '#222222', 'no stale value survives the rebuild');
});

test('buildLocateRegistry: perf — linear over a large face set, well under threshold', () => {
  const apps = {}, pkgmap = {};
  for (let a = 0; a < 40; a++) {
    const app = 'app' + a;
    apps[app] = { label: app, faces: [] };
    pkgmap[app] = {};
    for (let f = 0; f < 40; f++) pkgmap[app]['face' + f] = { fg: '#abcdef', bg: null, inherit: null, source: 'user' };
  }
  const start = process.hrtime.bigint();
  const reg = buildLocateRegistry(apps, pkgmap, {}, MAP);
  const ms = Number(process.hrtime.bigint() - start) / 1e6;
  assert.equal(Object.keys(reg).length, 1600);
  assert.ok(ms < 50, `build took ${ms.toFixed(2)}ms, expected < 50ms`);
});
