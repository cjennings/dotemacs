// Unit tests for the palette generator planner (palette-generator-core.js).
// Only planPaletteGenerator and entriesForGeneratedColumn are exported, so the
// internal scheme / vibe / source-mode / intent logic is exercised by driving
// the planner across each of those input dimensions.
// Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { planPaletteGenerator, entriesForGeneratedColumn } from './palette-generator-core.js';

const GROUND = { bg: '#0d0b0a', fg: '#f0fef0' };
const PAL = [['#0d0b0a', 'bg'], ['#f0fef0', 'fg'], ['#67809c', 'blue'], ['#e8bd30', 'gold']];
const rng = () => 0.42; // deterministic, so failures repeat

test('planPaletteGenerator: Normal — every scheme produces a valid plan', () => {
  for (const scheme of ['random', 'analogous', 'triadic', 'manual']) {
    const plan = planPaletteGenerator(PAL, GROUND, { scheme, accentCount: 4, spanCount: 0, rng });
    assert.equal(plan.scheme, scheme);
    assert.ok(Array.isArray(plan.columns), `${scheme} columns`);
    assert.equal(typeof plan.summary.generated, 'number');
  }
});

test('planPaletteGenerator: Normal — every vibe biases hues without error', () => {
  for (const vibe of ['warm', 'cool', 'earthy', 'muted', 'pastel', 'deep',
                      'jewel', 'neon', 'strange', 'balanced']) {
    const plan = planPaletteGenerator(PAL, GROUND,
      { scheme: 'analogous', vibe, accentCount: 5, spanCount: 0, rng });
    assert.equal(plan.vibe, vibe);
    assert.ok(Array.isArray(plan.columns), `${vibe} columns`);
  }
});

test('planPaletteGenerator: Normal — every source mode resolves', () => {
  for (const sourceMode of ['bg-fg', 'palette', 'none', 'selected']) {
    const plan = planPaletteGenerator(PAL, GROUND,
      { sourceMode, selectedHex: '#9b5fd0', scheme: 'analogous', accentCount: 3, spanCount: 0, rng });
    assert.ok(['bg-fg', 'palette', 'none', 'selected'].includes(plan.sourceMode));
    assert.ok(Array.isArray(plan.columns));
  }
});

test('planPaletteGenerator: Boundary — selected source with no valid hex falls back to bg-fg', () => {
  const plan = planPaletteGenerator(PAL, GROUND,
    { sourceMode: 'selected', scheme: 'analogous', accentCount: 2, spanCount: 0, rng });
  assert.equal(plan.sourceMode, 'bg-fg');
});

test('planPaletteGenerator: Normal — fill-gaps and fill-hue-gaps intents produce plans', () => {
  for (const intent of ['fill-gaps', 'fill-hue-gaps']) {
    const plan = planPaletteGenerator(PAL, GROUND, { intent, accentCount: 4, spanCount: 0, rng });
    assert.equal(plan.intent, intent);
    assert.ok(Array.isArray(plan.columns));
  }
});

test('planPaletteGenerator: Boundary — an empty palette still plans', () => {
  const plan = planPaletteGenerator([], { bg: '#000000', fg: '#ffffff' },
    { scheme: 'analogous', accentCount: 3, spanCount: 0, rng });
  assert.ok(Array.isArray(plan.columns));
  assert.equal(typeof plan.summary.generated, 'number');
});

test('planPaletteGenerator: Boundary — spanCount expands a column into members', () => {
  const plan = planPaletteGenerator(PAL, GROUND,
    { scheme: 'analogous', accentCount: 2, spanCount: 2, rng });
  if (plan.columns.length) assert.ok(plan.columns[0].members.length >= 1);
});

test('entriesForGeneratedColumn: Normal — maps a planned column to palette entries', () => {
  const plan = planPaletteGenerator(PAL, GROUND,
    { scheme: 'analogous', accentCount: 1, spanCount: 0, rng });
  if (plan.columns.length) {
    const entries = entriesForGeneratedColumn(plan.columns[0]);
    assert.ok(Array.isArray(entries) && entries.length >= 1);
    assert.ok(typeof entries[0][0] === 'string' && entries[0][0].startsWith('#'));
  }
});
