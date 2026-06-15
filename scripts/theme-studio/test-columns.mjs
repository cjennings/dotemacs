// Unit tests for the color-column model (app-core.js): grouping a flat palette
// into stable structural columns, regenerating a column's ramp, ranking members
// by lightness, and planning assignment re-point across a regenerate. Pure, no
// DOM. Run: node --test scripts/theme-studio/

import { test } from 'node:test';
import assert from 'node:assert/strict';
import { columnsFromPalette, regenColumn, groundRoleOfEntry, rankByLightness, stepRepointPlan, sortColumns } from './app-core.js';

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

test('columnsFromPalette: Boundary - legacy color-N entries become separate base columns', () => {
  const pal = [['#111111', 'color-22'], ['#222222', 'color-23'], ['#333333', 'color-129']];
  const { columns } = columnsFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.deepEqual(columns.map(f => f.column), ['color-22', 'color-23', 'color-129']);
  assert.deepEqual(columns.map(f => f.members.map(m => m.name)), [['color-22'], ['color-23'], ['color-129']]);
});

test('columnsFromPalette: Boundary - explicit color-N column ids are preserved', () => {
  const pal = [['#111111', 'color-22', 'captured'], ['#222222', 'color-23', 'captured']];
  const { columns } = columnsFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.equal(columns.length, 1);
  assert.equal(columns[0].column, 'captured');
  assert.deepEqual(columns[0].members.map(m => m.name), ['color-22', 'color-23']);
});

test('columnsFromPalette: Boundary - external numeric color names group by text stem', () => {
  const pal = [['#0000ee', 'blue1'], ['#0000cd', 'blue2'], ['#bebebe', 'grey80'], ['#c0c0c0', 'grey81'], ['#cd69c9', 'orchid3']];
  const { columns } = columnsFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.deepEqual(columns.map(f => f.column), ['blue', 'grey', 'orchid']);
  assert.deepEqual(columnOf(columns, 'blue1').members.map(m => m.name), ['blue1', 'blue2']);
  assert.deepEqual(columnOf(columns, 'grey80').members.map(m => m.name), ['grey80', 'grey81']);
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

test('columnsFromPalette: Boundary - ground entries and ground+N steps stay out of normal columns', () => {
  const pal = [['#0d0b0a', 'bg', 'ground'], ['#444444', 'ground+1', 'ground'], ['#67809c', 'blue']];
  const { ground, columns } = columnsFromPalette(pal, { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.ok(ground.some(g => g.hex.toLowerCase() === '#0d0b0a'));
  assert.ok(!columns.some(f => f.members.some(m => m.name === 'bg' || m.name === 'ground+1')));
});

test('columnsFromPalette: Boundary - imported bg-like names are not ground just because their hex matches bg', () => {
  const pal = [['#0d0b0a', 'bg2'], ['#0d0b0a', 'bg-alt'], ['#0d0b0a', 'bg', 'ground'], ['#f0fef0', 'fg', 'ground']];
  const { ground, columns } = columnsFromPalette(pal, { bg: '#0d0b0a', fg: '#f0fef0' });
  assert.deepEqual(ground.map(g => [g.role, g.name]), [['bg', 'bg'], ['fg', 'fg']]);
  assert.ok(columnOf(columns, 'bg2'), 'bg2 remains in a normal imported color column');
  assert.ok(columnOf(columns, 'bg-alt'), 'bg-alt remains in a normal imported color column');
  assert.deepEqual(columns.map(f => f.column), ['bg2', 'bg-alt']);
});

test('columnsFromPalette: Boundary - bg and fg prefixed legacy names are independent bases, not generated steps', () => {
  const pal = [['#101010', 'bg-1'], ['#202020', 'bg-2'], ['#eeeeee', 'fg-1'], ['#dddddd', 'fg-alt']];
  const { columns } = columnsFromPalette(pal, { bg: '#000000', fg: '#ffffff' });
  assert.deepEqual(columns.map(f => f.column), ['bg-1', 'bg-2', 'fg-1', 'fg-alt']);
  assert.deepEqual(columns.map(f => f.members.map(m => m.name)), [['bg-1'], ['bg-2'], ['fg-1'], ['fg-alt']]);
});

test('groundRoleOfEntry: Boundary - exact ground roles only, not bg-prefix names', () => {
  const ground = { bg: '#0d0b0a', fg: '#f0fef0' };
  assert.equal(groundRoleOfEntry(['#0d0b0a', 'bg'], ground), 'bg');
  assert.equal(groundRoleOfEntry(['#0d0b0a', 'ground'], ground), 'bg');
  assert.equal(groundRoleOfEntry(['#0d0b0a', 'bg2'], ground), null);
  assert.equal(groundRoleOfEntry(['#0d0b0a', 'bg-alt'], ground), null);
  assert.equal(groundRoleOfEntry(['#444444', 'ground+1'], ground), 'step');
  assert.equal(groundRoleOfEntry(['#555555', 'ground-1'], ground), 'step');
});

test('groundRoleOfEntry: Boundary - renamed entries with the ground column id remain steps', () => {
  const ground = { bg: '#ffffff', fg: '#000000' };
  assert.equal(groundRoleOfEntry(['#777777', 'renamed-middle', 'ground'], ground), 'step');
});

// --- regenColumn ------------------------------------------------------------

test('regenColumn: Normal - n steps each side plus the base, ordered by offset', () => {
  const r = regenColumn('#67809c', 2);
  assert.equal(r.members.length, 5);
  assert.deepEqual(r.members.map(m => m.offset), [-2, -1, 0, 1, 2]);
  assert.equal(r.members.find(m => m.offset === 0).hex, '#67809c');
});

test('regenColumn: Boundary - n=0 is the base alone', () => {
  const r = regenColumn('#67809c', 0);
  assert.deepEqual(r.members, [{ hex: '#67809c', offset: 0, clamped: false }]);
});

test('regenColumn: Boundary - span count is capped at eight per side', () => {
  const r = regenColumn('#67809c', 10);
  assert.equal(r.members.length, 17);
  assert.deepEqual(r.members.map(m => m.offset), [-8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8]);
});

test('regenColumn: Error - a malformed base returns a structured bad-hex', () => {
  assert.deepEqual(regenColumn('nope', 2), { members: [], error: 'bad-hex' });
});

test('regenColumn: Boundary - generated pure white and black endpoint steps are skipped', () => {
  assert.ok(!regenColumn('#fefefe', 8).members.some(m => m.offset !== 0 && m.hex === '#ffffff'));
  assert.ok(!regenColumn('#010101', 8).members.some(m => m.offset !== 0 && m.hex === '#000000'));
});

test('regenColumn: Normal - changing span count redistributes steps between endpoint and base', () => {
  const one = regenColumn('#67809c', 1).members.find(m => m.offset === 1).hex;
  const two = regenColumn('#67809c', 2).members.find(m => m.offset === 1).hex;
  assert.notEqual(one, two);
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

// --- regenColumn ground bounds (task: spans stop at bg/fg) -------------------
const _lum = h => { const n=parseInt(h.slice(1),16),r=(n>>16&255)/255,g=(n>>8&255)/255,b=(n&255)/255; const f=c=>c<=0.03928?c/12.92:((c+0.055)/1.055)**2.4; return 0.2126*f(r)+0.7152*f(g)+0.0722*f(b); };

test('regenColumn: Normal - ground-bounded span stays within the bg/fg endpoints', () => {
  const bg = '#101010', fg = '#f0f0f0';
  const members = regenColumn('#67809c', 4, { ground: { bg, fg } }).members;
  const lo = _lum(bg), hi = _lum(fg);
  assert.ok(members.every(m => _lum(m.hex) >= lo - 1e-6 && _lum(m.hex) <= hi + 1e-6),
    'every generated member sits within [bg, fg] luminance');
});

test('regenColumn: Boundary - a near-black bg yields no duplicate pure-black tiles', () => {
  const members = regenColumn('#67809c', 8, { ground: { bg: '#000000', fg: '#ffffff' } }).members;
  assert.ok(!members.some(m => m.offset !== 0 && (m.hex === '#000000' || m.hex === '#ffffff')),
    'pure endpoints are not duplicated as generated steps');
});

test('regenColumn: Boundary - no ground falls back to the black/white span', () => {
  assert.equal(regenColumn('#67809c', 2).members.length, 5);
});
