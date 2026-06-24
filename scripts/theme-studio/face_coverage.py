#!/usr/bin/env python3
"""Build (and diff) face-coverage.org from a face-coverage-dump.el JSON dump.

The worklist lists every known face -- the live Emacs face-list unioned with
everything theme-studio manages -- grouped into three tiers:

  emacs-core     standalone built-in faces (frame chrome, cursor, region, ...)
  emacs-general  built-in Emacs subsystems (org, gnus, erc, diff, vc, ...)
  <package>      one heading per third-party elpa package

Tier is decided by where each face's defface lives: /usr/share/emacs is built-in,
elpa is a package. Each face carries its docstring; each bucket carries its
customization-group doc or package summary.

Usage:
  python3 face_coverage.py --data DUMP.json --out face-coverage.org
  python3 face_coverage.py --data DUMP.json --compare face-coverage.org

The builder is deterministic given a dump. The --compare mode regenerates in
memory and reports the coverage delta against an existing org file (newly
covered, newly present, disappeared, per-tier totals) without writing.
"""

import argparse
import collections
import datetime
import json
import os
import re
import sys

import generate  # sibling module: UI_FACES, CATS

HERE = os.path.dirname(os.path.abspath(__file__))

# Faces that belong to emacs-core (fundamental display faces), even though their
# name prefix would otherwise route them to a subsystem bucket.
CORE_HINT = {
    'default', 'cursor', 'region', 'secondary-selection', 'highlight', 'hl-line',
    'shadow', 'match', 'fringe', 'minibuffer-prompt', 'mode-line', 'mode-line-inactive',
    'mode-line-highlight', 'mode-line-emphasis', 'mode-line-buffer-id', 'mode-line-active',
    'header-line', 'header-line-highlight', 'vertical-border', 'window-divider',
    'window-divider-first-pixel', 'window-divider-last-pixel', 'line-number',
    'line-number-current-line', 'line-number-major-tick', 'line-number-minor-tick',
    'isearch', 'isearch-fail', 'isearch-group-1', 'isearch-group-2', 'lazy-highlight',
    'show-paren-match', 'show-paren-mismatch', 'show-paren-match-expression', 'link',
    'link-visited', 'error', 'warning', 'success', 'tooltip', 'trailing-whitespace',
    'fill-column-indicator', 'escape-glyph', 'homoglyph', 'nobreak-space', 'nobreak-hyphen',
    'glyphless-char', 'button', 'help-key-binding', 'separator-line', 'scroll-bar', 'tool-bar',
    'menu', 'border', 'internal-border', 'child-frame-border', 'mouse', 'mouse-drag-and-drop-region',
    'bold', 'italic', 'bold-italic', 'underline', 'fixed-pitch', 'fixed-pitch-serif',
    'variable-pitch', 'variable-pitch-text', 'next-error', 'query-replace',
    'completions-common-part', 'completions-first-difference', 'blink-matching-paren-offscreen',
    'tty-menu-disabled-face', 'tty-menu-enabled-face', 'tty-menu-selected-face',
}

# Extra subsystem/package buckets beyond the package-inventory keys, so every
# face routes to a named bucket rather than falling into emacs-core.
EXTRA_FAMILIES = {
    'font-lock', 'org', 'org-agenda', 'org-block', 'org-table', 'org-habit', 'org-document',
    'org-priority', 'gnus', 'gnus-group', 'gnus-summary', 'gnus-header', 'gnus-cite',
    'gnus-server', 'gnus-splash', 'gnus-emphasis', 'gnus-signature', 'gnus-button', 'erc',
    'message', 'message-header', 'custom', 'diff', 'smerge', 'ediff', 'dired', 'image-dired',
    'wdired', 'info', 'vc', 'shr', 'eww', 'epa', 'package', 'compilation', 'outline',
    'completions', 'widget', 'apropos', 'change-log', 'calendar', 'diary', 'holiday',
    'which-key', 'tab-bar', 'tab-line', 'ansi-color', 'xref', 'comint', 'shell', 'sh',
    'makefile', 'eshell', 'term', 'help', 'eldoc', 'ert', 'kmacro', 'gud', 'bookmark',
    'ibuffer', 'grep', 'tabulated-list', 'flymake', 'flyspell', 'whitespace',
    'rainbow-delimiters', 'tmr', 'alert', 'breakpoint', 'mm', 'treesit', 'image', 'icon',
    'auto', 'buffer', 'browse', 'confusingly', 'adob', 'log', 'next', 'read', 'table',
    'rectangle', 'file', 'ffap', 'edmacro', 'elisp', 'doc', 'markdown', 'lsp', 'abbrev',
    'which-func', 'git-gutter', 'git-commit', 'twentyfortyeight', 'yas', 'edit-indirect',
}

# Curated descriptions for buckets whose group/package docs don't resolve.
DESC_FALLBACK = {
    'completions': 'Faces for the default *Completions* buffer.',
    'twentyfortyeight': 'Tile faces for the 2048 game.',
    'yas': 'Faces for YASnippet template fields.',
    'json-mode': 'Major mode for editing JSON.',
    'adob': 'auto-dim-other-buffers: dimmed inactive windows.',
    'diary': 'Faces for diary entries in the calendar.',
    'holiday': 'Faces for holidays in the calendar.',
    'mm': 'MIME handling faces (gnus/mm).',
    'emacs-core': 'Standalone built-in faces: frame chrome, cursor, region, mode line, '
                  'search, line numbers, base typography.',
    'emacs-general': 'Built-in Emacs subsystems, one child per subsystem.',
}


def clean_doc(doc):
    """First non-empty line of DOC, smart-quotes normalized, whitespace collapsed."""
    if not doc:
        return ''
    line = next((ln for ln in doc.split('\n') if ln.strip()), '')
    line = (line.replace('‘', "'").replace('’', "'")
                .replace('“', '"').replace('”', '"').replace('—', '-'))
    return re.sub(r'[ \t]+', ' ', line).strip()


def load_managed():
    """Return the set of faces theme-studio already themes (its three tiers)."""
    bt = open(os.path.join(HERE, 'build-theme.el')).read()
    fontlock = set(re.findall(r'font-lock-[a-z-]+', bt))
    ui = {f[0] for f in generate.UI_FACES}
    inv = json.load(open(os.path.join(HERE, 'package-inventory.json')))
    pkg = {f for faces in inv.values() for f in faces if isinstance(f, str)}
    managed = fontlock | ui | pkg | {'default', 'fixed-pitch', 'variable-pitch'}
    return managed, fontlock, ui, pkg, inv


# Built-in source files whose faces are core display faces, not a subsystem;
# an unrecognized face from one of these stays in emacs-core rather than
# spawning an odd subsystem bucket under emacs-general.
CORE_FILES = {'faces', 'frame'}


def path_kind(path):
    """Classify a defface source PATH into a coarse origin kind.
    Returns one of: 'none' (no path), 'elpa', 'user', 'builtin', 'other'.
    Shared by bucket_from_source and bucket_of_source, which each map the kind
    to their own vocabulary."""
    if not path:
        return 'none'
    if '/elpa/' in path:
        return 'elpa'
    if '/.emacs.d/modules' in path:
        return 'user'
    if path.startswith('/usr/share/emacs') or path.startswith('/usr/lib/emacs'):
        return 'builtin'
    return 'other'


def bucket_from_source(path):
    """Derive a bucket name from a face's defface file, for faces that match no
    known family. elpa -> the package dir name (version stripped); built-in ->
    the source file basename; otherwise emacs-core (can't tell)."""
    kind = path_kind(path)
    if kind == 'elpa':
        pkgdir = path.split('/elpa/', 1)[1].split('/', 1)[0]
        return re.sub(r'-[0-9].*$', '', pkgdir) or 'emacs-core'
    if kind == 'user':
        return 'user-config'
    if kind == 'builtin':
        base = os.path.basename(path)
        base = base[:-3] if base.endswith('.el') else base
        return 'emacs-core' if base in CORE_FILES else base
    return 'emacs-core'  # 'none' or 'other'


def make_group_of(families, src):
    fams = sorted(families, key=len, reverse=True)

    def group_of(f):
        if f.startswith('bg:erc') or f.startswith('fg:erc'):
            return 'erc-ansi'
        if f in CORE_HINT:
            return 'emacs-core'
        if f.startswith('font-lock'):
            return 'font-lock'
        for p in fams:
            if f == p or (f.startswith(p) and len(f) > len(p) and f[len(p)] in '-:/'):
                return p
        if f.lower().startswith('info-'):
            return 'info'
        # Unrecognized: route by where the defface lives so a newly-loaded
        # package buckets itself instead of falling into emacs-core.
        return bucket_from_source(src.get(f, ''))
    return group_of


def bucket_of_source(path):
    return {'none': 'unloaded', 'elpa': 'elpa', 'user': 'user',
            'builtin': 'builtin', 'other': 'other'}[path_kind(path)]


def classify(name, items, src, pkgfaces):
    """core / general / package for a bucket, by its faces' defface source."""
    if name == 'emacs-core':
        return 'core'
    c = collections.Counter(bucket_of_source(src.get(f, '')) for f in items)
    elpa, builtin, user, other = c['elpa'], c['builtin'], c['user'], c['other']
    if elpa + builtin + user + other == 0:
        return 'package' if any(f in pkgfaces for f in items) else 'general'
    if elpa >= max(builtin, user, other):
        return 'package'
    if other > builtin and other >= elpa:
        return 'package'
    return 'general'


def resolve_desc(bucket, groups, packages):
    """One-line description for BUCKET via group doc / package summary / parent."""
    if bucket in DESC_FALLBACK:
        # umbrella tiers and curated fills take precedence over a weak group hit
        if bucket in ('emacs-core', 'emacs-general'):
            return DESC_FALLBACK[bucket]
    for cand in (bucket, bucket + '-faces', bucket + 's', bucket + '-mode', bucket + '-mode-faces'):
        if groups.get(cand):
            return clean_doc(groups[cand])
    for cand in (bucket, bucket + '-mode'):
        if packages.get(cand):
            return clean_doc(packages[cand])
    if '-' in bucket:
        parent = bucket.rsplit('-', 1)[0]
        if groups.get(parent):
            return clean_doc(groups[parent])
    return DESC_FALLBACK.get(bucket, '')


def status(done, total):
    return 'DONE' if done == total else ('TODO' if done == 0 else 'DOING')


def build(data, today):
    faces_raw = data['faces']
    docs = {row[0]: ('' if row[1] in (None, ':null') else clean_doc(row[1])) for row in faces_raw}
    src = {row[0]: ('' if row[2] in (None, ':null') else row[2]) for row in faces_raw}
    groups_doc = data.get('groups', {})
    packages = data.get('packages', {})

    managed, fontlock, ui, pkgfaces, inv = load_managed()
    universe = sorted(set(docs.keys()) | managed)

    families = set(inv.keys()) | EXTRA_FAMILIES
    group_of = make_group_of(families, src)
    groups = collections.defaultdict(list)
    for f in universe:
        groups[group_of(f)].append(f)

    cls = {k: classify(k, v, src, pkgfaces) for k, v in groups.items()}
    done = lambda items: sum(1 for f in items if f in managed)

    gen_groups = sorted(k for k in groups if cls[k] == 'general')
    pkg_groups = sorted(k for k in groups if cls[k] == 'package')
    gen_faces = [f for k in gen_groups for f in groups[k]]
    tot_done = done(universe)
    ndoc = sum(1 for f in universe if docs.get(f))

    out = []

    def emit_desc(bucket, stars):
        d = resolve_desc(bucket, groups_doc, packages)
        if d:
            out.append(' ' * (stars + 1) + d)

    def emit_face(f, stars):
        out.append('%s %s %s' % ('*' * stars, 'DONE' if f in managed else 'TODO', f))
        if docs.get(f):
            out.append(' ' * (stars + 1) + docs[f])

    out += [
        '#+TITLE: theme-studio — face coverage master list',
        '#+DATE: %s' % today,
        '#+TODO: TODO DOING | DONE',
        '#+STARTUP: overview',
        '',
        'Every known face (live Emacs face-list union everything theme-studio manages). DONE = the',
        'studio already themes it; TODO = not yet. Three top-level tiers:',
        '- emacs-core: the standalone built-in faces (frame chrome, cursor, region, mode line, search).',
        '- emacs-general: built-in Emacs subsystems (org, gnus, erc, diff, vc, custom, ...), one child each.',
        '- one heading per third-party package installed from elpa (magit, vertico, consult, ...).',
        "Tier is decided by where each face's defface lives: /usr/share/emacs = built-in, elpa = package.",
        'The line under each bucket is its group/package description; the line under each face is its',
        'Emacs docstring (first line), where one exists.',
        '',
        'Totals: %d / %d faces covered; %d carry a docstring. Tiers: core 1, general %d, packages %d.'
        % (tot_done, len(universe), ndoc, len(gen_groups), len(pkg_groups)),
        'Coverage tiers in the studio: syntax font-lock=%d, UI tier=%d, package inventory=%d (%d packages).'
        % (len(fontlock), len(ui), len(pkgfaces), len(inv)),
        '',
        'Mechanism to close a TODO: core/UI faces -> UI_FACES in generate.py; package + subsystem faces',
        '-> package-inventory.json (regenerable via build-inventory.el / app_inventory.py).',
        '',
    ]

    core = sorted(groups['emacs-core'])
    out.append('* %s emacs-core [%d/%d]' % (status(done(core), len(core)), done(core), len(core)))
    emit_desc('emacs-core', 1)
    for f in core:
        emit_face(f, 2)
    out.append('')

    out.append('* %s emacs-general [%d/%d]'
               % (status(done(gen_faces), len(gen_faces)), done(gen_faces), len(gen_faces)))
    emit_desc('emacs-general', 1)
    for k in gen_groups:
        items = sorted(groups[k])
        out.append('** %s %s [%d/%d]' % (status(done(items), len(items)), k, done(items), len(items)))
        emit_desc(k, 2)
        for f in items:
            emit_face(f, 3)
    out.append('')

    for k in pkg_groups:
        items = sorted(groups[k])
        out.append('* %s %s [%d/%d]' % (status(done(items), len(items)), k, done(items), len(items)))
        emit_desc(k, 1)
        for f in items:
            emit_face(f, 2)
        out.append('')

    summary = {
        'total': (tot_done, len(universe)),
        'core': (done(core), len(core)),
        'general': (done(gen_faces), len(gen_faces)),
        'packages': len(pkg_groups),
    }
    return '\n'.join(out), summary


def parse_states(text):
    """face -> 'DONE'/'TODO' from an org worklist (face headings have no cookie)."""
    states = {}
    for m in re.finditer(r'^\*+ (TODO|DONE) (\S+)$', text, re.M):
        states[m.group(2)] = m.group(1)
    return states


def compare(old_text, new_text):
    old, new = parse_states(old_text), parse_states(new_text)
    newly_covered = sorted(f for f in new if new[f] == 'DONE' and old.get(f) == 'TODO')
    newly_present = sorted(set(new) - set(old))
    disappeared = sorted(set(old) - set(new))
    old_done = sum(1 for s in old.values() if s == 'DONE')
    new_done = sum(1 for s in new.values() if s == 'DONE')
    return {
        'newly_covered': newly_covered,
        'newly_present': newly_present,
        'disappeared': disappeared,
        'old': (old_done, len(old)),
        'new': (new_done, len(new)),
    }


def main(argv):
    ap = argparse.ArgumentParser(description='Build or diff face-coverage.org')
    ap.add_argument('--data', default='/tmp/face-coverage-data.json',
                    help='JSON dump from face-coverage-dump.el')
    ap.add_argument('--out', default=os.path.join(HERE, 'face-coverage.org'),
                    help='org file to write (build mode)')
    ap.add_argument('--compare', metavar='ORG',
                    help='regenerate in memory and report the delta against ORG, do not write')
    ap.add_argument('--date', default=None, help='override the #+DATE stamp (YYYY-MM-DD)')
    args = ap.parse_args(argv)

    data = json.load(open(args.data))
    today = args.date or datetime.date.today().isoformat()
    text, summary = build(data, today)

    if args.compare:
        old_text = open(args.compare).read()
        d = compare(old_text, text)
        print('coverage: %d/%d -> %d/%d' % (d['old'][0], d['old'][1], d['new'][0], d['new'][1]))
        print('newly covered (%d): %s' % (len(d['newly_covered']), ' '.join(d['newly_covered']) or '-'))
        print('newly present (%d): %s' % (len(d['newly_present']), ' '.join(d['newly_present']) or '-'))
        print('disappeared (%d): %s' % (len(d['disappeared']), ' '.join(d['disappeared']) or '-'))
        return 0

    with open(args.out, 'w') as fh:
        fh.write(text)
    print('wrote %s — %d/%d covered, core %d/%d, general %d/%d, %d package groups'
          % (args.out, summary['total'][0], summary['total'][1], summary['core'][0],
             summary['core'][1], summary['general'][0], summary['general'][1], summary['packages']))
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
