// Unit tests for the color-column model (app-core.js): grouping a flat palette
// into stable structural columns, regenerating a column's ramp, ranking members
// by lightness, and planning assignment re-point across a regenerate. Pure, no
// DOM. Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { columnsFromPalette, regenColumn, rankByLightness, stepRepointPlan, sortColumns } from './app-core.js';

const columnOf = (columns, name) => columns.find(f => f.members.some(m => m.name === name));

// --- columnsFromPalette ----------------------------------------------------

test('columnsFromPalette: Normal - generated names group by column stem', () => {
  const pal = [['#223344', 'blue-1'], ['#67809c', 'blue'], ['#b2c3cc', 'blue+1']];
  const { ground, columns } = columnsFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(ground.length, 2, 'ground strip carries bg and fg');
  assert.equal(columns.length, 1);
  assert.deepEqual(columns[0].members.map(m => m.name), ['blue-1', 'blue', 'blue+1']);
});

test('columnsFromPalette: Normal - different stems stay in different columns even with similar colors', () => {
  const pal = [['#67809c', 'blue'], ['#6a829e', 'steel']];
  const { columns } = columnsFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(columns.length, 2);
  assert.notEqual(columnOf(columns, 'blue'), columnOf(columns, 'steel'));
});

test('columnsFromPalette: Boundary - a stable third-field column id survives rename', () => {
  const pal = [['#223344', 'blue-1', 'blue'], ['#67809c', 'renamed-base', 'blue'], ['#b2c3cc', 'wild-card', 'blue']];
  const { columns } = columnsFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(columns.length, 1);
  assert.equal(columns[0].column, 'blue');
  assert.deepEqual(columns[0].members.map(m => m.name), ['blue-1', 'renamed-base', 'wild-card']);
});

test('columnsFromPalette: Boundary - legacy two-field entries fall back to name stem', () => {
  const pal = [['#875f00', 'yellow-1'], ['#d7af5f', 'yellow'], ['#646d14', 'green-1'], ['#a4ac64', 'green']];
  const { columns } = columnsFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(columns.length, 2);
  assert.ok(columnOf(columns, 'yellow'));
  assert.ok(columnOf(columns, 'green'));
});

test('columnsFromPalette: Normal - palette order controls column order', () => {
  const pal = [['#67809c', 'blue'], ['#e8bd30', 'gold'], ['#5d9b86', 'green']];
  const { columns } = columnsFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.deepEqual(columns.map(f => f.members[0].name), ['blue', 'gold', 'green']);
});

test('columnsFromPalette: Boundary - ground hex absent from the palette still forms the strip', () => {
  const pal = [['#67809c', 'blue']];
  const { ground } = columnsFromPalette(pal, { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.equal(ground.length, 2);
  assert.ok(ground.some(g => g.hex.toLowerCase() === '#0d0b0a' && g.role === 'bg'));
  assert.ok(ground.some(g => g.role === 'fg'));
});

test('columnsFromPalette: Boundary - ground entries and ground-N steps stay out of normal columns', () => {
  const pal = [['#0d0b0a', 'bg', 'ground'], ['#444444', 'ground-1', 'ground'], ['#67809c', 'blue']];
  const { ground, columns } = columnsFromPalette(pal, { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.ok(ground.some(g => g.hex.toLowerCase() === '#0d0b0a'));
  assert.ok(!columns.some(f => f.members.some(m => m.name === 'bg' || m.name === 'ground-1')));
});

// --- regenColumn ------------------------------------------------------------

test('regenColumn: Normal - n steps each side plus the base, ordered by offset', () => {
  const r = regenColumn('#67809c', 2);
  assert.equal(r.members.length, 5);
  assert.deepEqual(r.members.map(m => m.offset), [-2, -1, 0, 1, 2]);
  assert.equal(r.members.find(m => m.offset === 0).hex, '#67809c');
});

test('regenColumn: Boundary - n=0 is the base alone, no ramp() clamp to 1', () => {
  const r = regenColumn('#67809c', 0);
  assert.deepEqual(r.members, [{ hex: '#67809c', offset: 0, clamped: false }]);
});

test('regenColumn: Error - a malformed base returns a structured bad-hex', () => {
  assert.deepEqual(regenColumn('nope', 2), { members: [], error: 'bad-hex' });
});

// --- rankByLightness --------------------------------------------------------

test('rankByLightness: Normal - offsets are signed distance from the base by lightness', () => {
  const members = regenColumn('#67809c', 2).members.map(m => m.hex);
  const ranked = rankByLightness(members, '#67809c');
  const base = ranked.find(m => m.hex === '#67809c');
  assert.equal(base.offset, 0);
  const sorted = [...ranked].sort((a, b) => a.offset - b.offset);
  assert.deepEqual(sorted.map(m => m.offset), [-2, -1, 0, 1, 2]);
});

test('rankByLightness: Boundary - a base not among the members ranks by nearest lightness', () => {
  const members = ['#222222', '#888888', '#dddddd'];
  const ranked = rankByLightness(members, '#8a8a8a');
  const mid = ranked.find(m => m.hex === '#888888');
  assert.equal(mid.offset, 0, 'nearest-lightness member is the base rank');
});

// --- stepRepointPlan --------------------------------------------------------

test('stepRepointPlan: Normal - surviving offsets map old->new, changed hex only', () => {
  const oldR = [{ hex: '#111111', offset: -1 }, { hex: '#222222', offset: 0 }, { hex: '#333333', offset: 1 }];
  const neu = [{ hex: '#111111', offset: -1 }, { hex: '#aaaaaa', offset: 0 }, { hex: '#444444', offset: 1 }];
  const { map, removed } = stepRepointPlan(oldR, neu);
  assert.deepEqual(removed, []);
  assert.deepEqual(map, [['#222222', '#aaaaaa'], ['#333333', '#444444']]);
});

test('stepRepointPlan: Boundary - an offset with no new counterpart is removed, not repointed', () => {
  const oldR = [{ hex: '#000033', offset: -3 }, { hex: '#222222', offset: 0 }];
  const neu = [{ hex: '#222222', offset: 0 }];
  const { map, removed } = stepRepointPlan(oldR, neu);
  assert.deepEqual(map, []);
  assert.deepEqual(removed, ['#000033']);
});

// --- sortColumns -----------------------------------------------------------

const column = (label, members) => ({ base: members[0], column: label, members: members.map((h, i) => ({ hex: h, name: label + i })) });

test('sortColumns: Normal - preserves first-seen column order', () => {
  const columns = [column('blue', ['#67809c']), column('gold', ['#e8bd30']), column('green', ['#5d9b86'])];
  assert.deepEqual(sortColumns(columns).map(f => f.column), ['blue', 'gold', 'green']);
});

test('sortColumns: Normal - preserves member order inside a column', () => {
  const members = ['#dddddd', '#222222', '#888888'];
  assert.deepEqual(sortColumns([column('gray', members)])[0].members.map(m => m.hex), members);
});
