// previews.js -- the bespoke per-package preview renderers, extracted from
// app.js. Pure preview HTML builders (ofs/os/previewLines + renderXxxPreview);
// they reference shared globals (PKGMAP, MAP, faceCss, effFg, ...) and are
// inlined into the page's single script element via the PREVIEWS_J token in app.js.
function ofs(app,face){const f=PKGMAP[app][face]||{},fg=effFg(pkgEffFg(app,face)),bg=pkgEffBg(app,face);return faceCss(f,fg,bg,{fontSize:(f.height||1),boxBg:bg||MAP['bg']});}
// The CSS for a UI-owned face rendered off any preview surface: effective fg
// (floored to the default fg) and bg, following the built-in UI inherit chain so
// the rendered color matches what the registry reports. The @ui counterpart to ofs.
function ulocateCss(face){const o=UIMAP[face]||{},fg=effFg(resolveUiAttr(face,'fg',UIMAP)),bg=resolveUiAttr(face,'bg',UIMAP)||null;return faceCss(o,fg,bg,{boxBg:bg||MAP['bg']});}
// previewSpan -- the one stateful locate adapter (preview-locate spec). Reads the
// live globals (PKGMAP / UIMAP / MAP), dispatches by the owner's surface to the
// package (ofs / PKGMAP) or @ui (UIMAP) style path, and emits the shared locate
// attributes: data-owner-app (the internal owner key), data-face, and the
// locate-onpane class when the owner is the pane currently viewed. TEXT is trusted
// preview HTML -- callers pre-escape entities, matching the old os() contract, so
// previewSpan does not re-escape it (that would double-escape &lt; etc.). os
// delegates here for package owners; an @ui or cross-package owner makes an
// off-pane, hover-only span.
function attresc(s){return esc(String(s)).replace(/"/g,'&quot;');}
function previewSpan(owner,face,text){
  const style=owner==='@ui'?ulocateCss(face):ofs(owner,face);
  const cls=isLocateOnPane(owner,curApp())?' class="locate-onpane"':'';
  const title=attresc(formatLocateTitle(locateFaceMeta(owner,face,LOCATE_REG)));
  return `<span data-owner-app="${owner}" data-face="${face}"${cls} title="${title}" style="${style}">${text}</span>`;
}
function os(app,face,txt){return previewSpan(app,face,txt);}
// Preview font stack: the embedded @font-face (family "ThemeStudioNerd",
// Symbols Nerd Font Mono inlined as a data: URI in styles.css) supplies the nerd
// glyphs; monospace supplies everything else. The family name is deliberately
// custom, NOT the real "Symbols Nerd Font Mono": when the @font-face name matches
// a font the user has installed system-wide, Chrome resolves the family to the
// local copy instead of our embedded one and the glyphs render as tofu (the
// embedded font only wins in environments without that system font, e.g. headless
// CI). A unique family name forces the embedded font. "ThemeStudioNerd" carries
// only icon glyphs, so plain text falls through to monospace and the layout is
// unchanged — only the nerd codepoints pull from the embedded font.
// NOTE: the family name is UNQUOTED here on purpose. PREVIEW_FONT is interpolated
// into inline style="..." attributes (previewLines, genericPreview, the mock
// frame), and a double-quoted family name inside a double-quoted attribute
// terminates the attribute early, silently dropping the font-family (the glyphs
// then fall back to monospace = tofu). A no-space identifier needs no quotes, so
// keep ThemeStudioNerd quote-free and never reintroduce a spaced/quoted name here.
const PREVIEW_FONT='ThemeStudioNerd,monospace';
// Shared wrapper for the line-based package previews: a nerd-font pre block.
// Each renderer builds its own L array of os(...) lines and returns previewLines(L).
function previewLines(L){return `<div style="padding:12px 16px;font:12pt/1.7 ${PREVIEW_FONT};white-space:pre">${L.join('\n')}</div>`;}
function renderOrgPreview(){const a='org-mode',L=[];
  L.push(os(a,'org-document-info-keyword','#+TITLE:')+' '+os(a,'org-document-title','Project Notes'));
  L.push(os(a,'org-document-info-keyword','#+AUTHOR:')+' '+os(a,'org-document-info','Craig Jennings'));
  L.push(os(a,'org-meta-line','#+STARTUP: overview'));
  L.push('');
  L.push(os(a,'org-level-1','* Inbox')+'  '+os(a,'org-tag',':work:')+os(a,'org-tag-group',':@office:'));
  L.push(os(a,'org-level-2','** ')+os(a,'org-todo','TODO')+os(a,'org-level-2',' Draft the spec')+' '+os(a,'org-priority','[#A]')+' '+os(a,'org-tag',':spec:'));
  L.push('   '+os(a,'org-special-keyword','SCHEDULED:')+' '+os(a,'org-date','&lt;2026-06-08 Sun&gt;')+'  '+os(a,'org-special-keyword','DEADLINE:')+' '+os(a,'org-date','&lt;2026-06-12 Thu&gt;'));
  L.push('   '+os(a,'org-drawer',':PROPERTIES:'));
  L.push('   '+os(a,'org-special-keyword',':ID:')+'       '+os(a,'org-property-value','abc-123-def'));
  L.push('   '+os(a,'org-drawer',':END:'));
  L.push('   '+os(a,'org-list-dt','- term ::')+' definition, with a '+os(a,'org-footnote','[fn:1]')+' note.');
  L.push('   '+os(a,'org-checkbox','[X]')+' done item   '+os(a,'org-checkbox-statistics-done','[2/2]'));
  L.push('   '+os(a,'org-checkbox','[ ]')+' open item   '+os(a,'org-checkbox-statistics-todo','[0/3]')+'  '+os(a,'org-warning','(!)'));
  L.push(os(a,'org-level-2','** ')+os(a,'org-done','DONE')+os(a,'org-headline-done',' Ship the tool'));
  L.push(os(a,'org-level-3','*** ')+os(a,'org-todo','TODO')+os(a,'org-headline-todo',' Heading three'));
  L.push(os(a,'org-level-4','**** four')+' / '+os(a,'org-level-5','***** five')+' / '+os(a,'org-level-6','****** six')+' / '+os(a,'org-level-7','******* seven')+' / '+os(a,'org-level-8','******** eight'));
  L.push('   Inline '+os(a,'org-code','=code=')+', '+os(a,'org-verbatim','~verbatim~')+', '+os(a,'org-inline-src-block','src_py{1+1}')+',');
  L.push('   a '+os(a,'org-link','[[https://gnu.org][link]]')+', a '+os(a,'org-target','&lt;&lt;target&gt;&gt;')+', a '+os(a,'org-macro','{{{macro}}}')+',');
  L.push('   a '+os(a,'org-cite','[cite:')+os(a,'org-cite-key','@knuth1984')+os(a,'org-cite',']')+', a date '+os(a,'org-sexp-date','&lt;%%(diary-float 6 5 2)&gt;')+'.');
  L.push('   '+os(a,'org-quote','#+begin_quote')+' a '+os(a,'org-verse','verse')+' line, latex '+os(a,'org-latex-and-related','$E = mc^2$')+'.');
  L.push('');
  L.push('   '+os(a,'org-block-begin-line','#+begin_src elisp'));
  L.push('   '+os(a,'org-block','  (message "hi")'));
  L.push('   '+os(a,'org-block-end-line','#+end_src'));
  L.push('');
  L.push('   '+os(a,'org-table-header','| name | hex     |'));
  L.push('   '+os(a,'org-table','|------+---------|'));
  L.push('   '+os(a,'org-table-row','| blue | #67809c |')+'  '+os(a,'org-formula',':=vsum(@2)'));
  L.push('   '+os(a,'org-column-title','Effort')+' '+os(a,'org-column','| 0:30 |')+'   '+os(a,'org-archived','* archived')+os(a,'org-ellipsis',' ...'));
  L.push('');
  L.push(os(a,'org-agenda-structure','Week-agenda (W23):'));
  L.push(os(a,'org-agenda-date','Monday      8 June 2026'));
  L.push(os(a,'org-agenda-date-today','Tuesday     9 June 2026')+' '+os(a,'org-agenda-current-time','10:24')+' '+os(a,'org-time-grid','----------'));
  L.push(os(a,'org-agenda-date-weekend','Saturday   13 June')+' / '+os(a,'org-agenda-date-weekend-today','wknd-today'));
  L.push('  '+os(a,'org-scheduled-previously','Sched.past:')+' overdue   '+os(a,'org-agenda-done','x done item'));
  L.push('  '+os(a,'org-scheduled','Scheduled:')+' a task   '+os(a,'org-scheduled-today','due today'));
  L.push('  '+os(a,'org-imminent-deadline','Deadline!')+' / '+os(a,'org-upcoming-deadline','upcoming')+' / '+os(a,'org-upcoming-distant-deadline','distant'));
  L.push('  '+os(a,'org-agenda-dimmed-todo-face','dimmed todo')+'  '+os(a,'org-agenda-diary','diary')+'  '+os(a,'org-agenda-clocking','clocking'));
  L.push('  '+os(a,'org-agenda-calendar-event','cal-event')+' / '+os(a,'org-agenda-calendar-sexp','cal-sexp')+' / '+os(a,'org-agenda-calendar-daterange','range'));
  L.push('  '+os(a,'org-agenda-structure-secondary','secondary')+' '+os(a,'org-agenda-structure-filter','filter')+' '+os(a,'org-agenda-restriction-lock','lock')+' '+os(a,'org-agenda-column-dateline','col-date'));
  L.push('  Filters: '+os(a,'org-agenda-filter-category','cat')+' '+os(a,'org-agenda-filter-tags','tags')+' '+os(a,'org-agenda-filter-effort','effort')+' '+os(a,'org-agenda-filter-regexp','re'));
  L.push('  '+os(a,'org-mode-line-clock','[0:45]')+' / '+os(a,'org-mode-line-clock-overrun','[OVER]')+'   '+os(a,'org-dispatcher-highlight','[d]ispatch'));
  return previewLines(L);
}
function renderMagitPreview(){const a='magit',L=[];
  L.push(os(a,'magit-header-line',' Magit: dotemacs ')+'  '+os(a,'magit-header-line-key','g')+os(a,'magit-header-line-log-select',' refresh'));
  L.push(os(a,'magit-head','Head:')+'     '+os(a,'magit-branch-current','main')+'  '+os(a,'magit-diff-revision-summary','Ship the tool'));
  L.push(os(a,'magit-head','Merge:')+'    '+os(a,'magit-branch-remote','origin/main')+'  '+os(a,'magit-branch-local','main'));
  L.push(os(a,'magit-head','Push:')+'     '+os(a,'magit-branch-remote-head','origin/main'));
  L.push(os(a,'magit-head','Upstream:')+' '+os(a,'magit-branch-upstream','origin/main')+'  '+os(a,'magit-branch-warning','(diverged)'));
  L.push('');
  L.push(os(a,'magit-section-heading','Untracked files')+' '+os(a,'magit-section-child-count','(2)'));
  L.push('  '+os(a,'magit-filename','notes.txt')+'   '+os(a,'magit-dimmed','(ignored sibling)'));
  L.push(os(a,'magit-section-highlight','  scratch.el   (highlighted row)'));
  L.push('');
  L.push(os(a,'magit-section-heading','Unstaged changes')+' '+os(a,'magit-section-child-count','(1)'));
  L.push(os(a,'magit-diff-file-heading','modified   generate.py'));
  L.push(os(a,'magit-diff-hunk-heading','@@ -1,4 +1,5 @@ def main'));
  L.push(os(a,'magit-diff-context','   unchanged context'));
  L.push(os(a,'magit-diff-removed','- old line')+os(a,'magit-diff-whitespace-warning','   '));
  L.push(os(a,'magit-diff-added','+ new line'));
  L.push('');
  L.push(os(a,'magit-section-heading','Staged changes')+'   '+os(a,'magit-diffstat-added','++++')+os(a,'magit-diffstat-removed','--'));
  L.push(os(a,'magit-diff-file-heading-highlight','modified   README.md   (highlighted heading)'));
  L.push(os(a,'magit-diff-hunk-heading-highlight','@@ hunk heading highlight @@'));
  L.push(os(a,'magit-diff-added-highlight','+ added highlight')+'   '+os(a,'magit-diff-removed-highlight','- removed highlight'));
  L.push(os(a,'magit-diff-context-highlight','  context highlight'));
  L.push('');
  L.push(os(a,'magit-section-heading','Stashes'));
  L.push('  '+os(a,'magit-refname-stash','stash@{0}')+'  '+os(a,'magit-refname-wip','wip')+'  '+os(a,'magit-refname-pullreq','pr/42')+'  '+os(a,'magit-refname','refs/heads/x'));
  L.push('');
  L.push(os(a,'magit-section-heading','Recent commits'));
  L.push(os(a,'magit-log-graph','* ')+os(a,'magit-hash','b5b1869f')+' '+os(a,'magit-log-date','06-08')+' '+os(a,'magit-log-author','Craig')+'  enlarge the picker');
  L.push(os(a,'magit-log-graph','* ')+os(a,'magit-hash','4fa5e995')+' '+os(a,'magit-log-date','06-07')+' '+os(a,'magit-log-author','Craig')+'  '+os(a,'magit-keyword','[feat]')+' picker');
  L.push(os(a,'magit-log-graph','* ')+os(a,'magit-hash','de07e01a')+' '+os(a,'magit-log-date','06-05')+' '+os(a,'magit-log-author','Craig')+'  '+os(a,'magit-tag','v0.3')+' '+os(a,'magit-keyword-squash','!squash'));
  L.push('');
  L.push(os(a,'magit-section-secondary-heading','Merge conflict')+'  '+os(a,'magit-diff-lines-heading','lines 10-14')+os(a,'magit-diff-lines-boundary','|'));
  L.push('  '+os(a,'magit-diff-conflict-heading','=======')+'  '+os(a,'magit-diff-conflict-heading-highlight','(hl)'));
  L.push('  '+os(a,'magit-diff-base','base')+'/'+os(a,'magit-diff-base-highlight','base-hl')+'  '+os(a,'magit-diff-our','ours')+'/'+os(a,'magit-diff-our-highlight','ours-hl')+'  '+os(a,'magit-diff-their','theirs')+'/'+os(a,'magit-diff-their-highlight','theirs-hl'));
  L.push('  '+os(a,'magit-diff-hunk-region','hunk-region')+'  '+os(a,'magit-diff-file-heading-selection','file-sel')+'  '+os(a,'magit-diff-hunk-heading-selection','hunk-sel')+'  '+os(a,'magit-section-heading-selection','sec-sel')+'  '+os(a,'magit-diff-revision-summary-highlight','rev-sum-hl'));
  L.push('');
  L.push(os(a,'magit-section-heading','Reflog'));
  L.push('  '+os(a,'magit-reflog-commit','commit')+' '+os(a,'magit-reflog-amend','amend')+' '+os(a,'magit-reflog-merge','merge')+' '+os(a,'magit-reflog-checkout','checkout')+' '+os(a,'magit-reflog-reset','reset')+' '+os(a,'magit-reflog-rebase','rebase')+' '+os(a,'magit-reflog-cherry-pick','cherry-pick')+' '+os(a,'magit-reflog-remote','remote')+' '+os(a,'magit-reflog-other','other'));
  L.push(os(a,'magit-section-heading','Rebase sequence'));
  L.push('  '+os(a,'magit-sequence-pick','pick')+' '+os(a,'magit-sequence-stop','stop')+' '+os(a,'magit-sequence-part','part')+' '+os(a,'magit-sequence-head','head')+' '+os(a,'magit-sequence-drop','drop')+' '+os(a,'magit-sequence-done','done')+' '+os(a,'magit-sequence-onto','onto')+' '+os(a,'magit-sequence-exec','exec'));
  L.push(os(a,'magit-section-heading','Bisect / Cherry / Process'));
  L.push('  '+os(a,'magit-bisect-good','good')+' '+os(a,'magit-bisect-bad','bad')+' '+os(a,'magit-bisect-skip','skip')+'   '+os(a,'magit-cherry-equivalent','equivalent')+' '+os(a,'magit-cherry-unmatched','unmatched'));
  L.push('  '+os(a,'magit-process-ok','OK')+' '+os(a,'magit-process-ng','NG')+'   '+os(a,'magit-mode-line-process','[fetch]')+' '+os(a,'magit-mode-line-process-error','[error]'));
  L.push(os(a,'magit-section-heading','Blame'));
  L.push(os(a,'magit-blame-margin','margin')+os(a,'magit-blame-heading',' b5b1869f '))
  L.push('  '+os(a,'magit-blame-hash','b5b1869f')+' '+os(a,'magit-blame-name','Craig')+' '+os(a,'magit-blame-date','2026-06-08')+' '+os(a,'magit-blame-summary','enlarge picker')+' '+os(a,'magit-blame-highlight','hl')+' '+os(a,'magit-blame-dimmed','dim'));
  L.push(os(a,'magit-section-heading','Signatures')+os(a,'magit-left-margin','  '));
  L.push('  '+os(a,'magit-signature-good','good')+' '+os(a,'magit-signature-bad','bad')+' '+os(a,'magit-signature-untrusted','untrusted')+' '+os(a,'magit-signature-expired','expired')+' '+os(a,'magit-signature-expired-key','expired-key')+' '+os(a,'magit-signature-revoked','revoked')+' '+os(a,'magit-signature-error','error'));
  return previewLines(L);}
function renderElfeedPreview(){const a='elfeed',L=[];
  L.push(os(a,'elfeed-search-filter-face','@6-months-ago +unread')+'   '+os(a,'elfeed-search-unread-count-face','3/120')+'   '+os(a,'elfeed-search-last-update-face','updated 02:24'));
  L.push('');
  L.push(os(a,'elfeed-search-date-face','2026-06-08')+'  '+os(a,'elfeed-search-feed-face','Planet Emacs')+'  '+os(a,'elfeed-search-unread-title-face','New release of Magit')+'  '+os(a,'elfeed-search-tag-face',':emacs:'));
  L.push(os(a,'elfeed-search-date-face','2026-06-07')+'  '+os(a,'elfeed-search-feed-face','LWN')+'  '+os(a,'elfeed-search-unread-title-face','Kernel 6.18 lands')+'  '+os(a,'elfeed-search-tag-face',':linux:'));
  L.push(os(a,'elfeed-search-date-face','2026-06-05')+'  '+os(a,'elfeed-search-feed-face','Hacker News')+'  '+os(a,'elfeed-search-title-face','Show HN: a theme editor')+'  '+os(a,'elfeed-search-tag-face',':show:'));
  L.push('');
  L.push(os(a,'elfeed-log-date-face','02:24:01')+'  '+os(a,'elfeed-log-info-level-face','INFO ')+' updated 12 feeds');
  L.push(os(a,'elfeed-log-date-face','02:24:02')+'  '+os(a,'elfeed-log-warn-level-face','WARN ')+' slow feed: example.com');
  L.push(os(a,'elfeed-log-date-face','02:24:03')+'  '+os(a,'elfeed-log-error-level-face','ERROR')+' failed: bad.example');
  L.push(os(a,'elfeed-log-date-face','02:24:04')+'  '+os(a,'elfeed-log-debug-level-face','DEBUG')+' parsed 340 entries');
  return previewLines(L);}
function renderDashboardPreview(){const a='dashboard',L=[];
  L.push(os(a,'dashboard-text-banner','        [ dashboard banner image ]'));
  L.push(os(a,'dashboard-banner-logo-title','Emacs: The Editor That Saves Your Soul'));
  L.push('');
  L.push(os(a,'dashboard-navigator',' Code   Files   Terminal  󰃭 Agenda'));
  L.push(os(a,'dashboard-navigator',' Feeds   Books  󰑴 Flashcards  󰝚 Music'));
  L.push(os(a,'dashboard-navigator',' Email   IRC   Telegram'));
  L.push(os(a,'dashboard-navigator',' Slack   Linear'));
  L.push('');
  L.push('');
  L.push(os(a,'dashboard-heading','Projects:'));
  L.push(os(a,'dashboard-items-face','    ~/'));
  L.push(os(a,'dashboard-items-face','    ~/.emacs.d/'));
  L.push(os(a,'dashboard-items-face','    ~/projects/work/'));
  L.push(os(a,'dashboard-items-face','    ~/org/roam/'));
  L.push(os(a,'dashboard-items-face','    ~/projects/home/'));
  L.push('');
  L.push(os(a,'dashboard-heading','Bookmarks'));
  L.push(os(a,'dashboard-items-face','    Cesar Aira, The Little Buddhist Monk & the Proof'));
  L.push(os(a,'dashboard-items-face','    Edward Abbey, The Fool’s Progress: An Honest Novel'));
  L.push(os(a,'dashboard-items-face','    Agatha Christie, The A.B.C. Murders'));
  L.push('');
  L.push(os(a,'dashboard-heading','Recent Files:'));
  L.push(os(a,'dashboard-items-face','    theme-theme.el'));
  L.push(os(a,'dashboard-items-face','    todo.org'));
  L.push(os(a,'dashboard-items-face','    theme-studio-palette-generator-spec.org'));
  return previewLines(L);}
function renderMu4ePreview(){const a='mu4e',L=[];
  const pad=(s,n)=>{s=String(s);return s.length>=n?s.slice(0,n):s+' '.repeat(n-s.length);};
  // One header line: the flags column in mu4e-header-marks-face, the rest of the
  // row in the message's state face (unread, replied, flagged, ...).
  const row=(flags,date,from,subj,face)=>
    os(a,'mu4e-header-marks-face',pad(flags,4))+os(a,face,pad(date,12)+pad(from,17)+subj);
  // status / context bar
  L.push(os(a,'mu4e-title-face','mu4e')+'   '+os(a,'mu4e-context-face','[Personal]')+'   '+os(a,'mu4e-ok-face','online')+'   '+os(a,'mu4e-warning-face','2 retrying')+'   '+os(a,'mu4e-modeline-face','[12/340]'));
  L.push('');
  // column header + the message list, one row per state
  L.push(os(a,'mu4e-header-title-face',pad('Flags',4)+pad('Date',12)+pad('From',17)+'Subject'));
  L.push(row('N','2026-06-14','Christine Park','Re: quarterly numbers','mu4e-unread-face'));
  L.push(row('','2026-06-13','Bob Lin','Lunch on Friday?','mu4e-header-face'));
  // current line at point: the whole row gets the highlight background
  L.push(os(a,'mu4e-header-highlight-face',row('R','2026-06-13','dev-list','merged the parser fix','mu4e-replied-face')));
  L.push(row('F','2026-06-12','Carol Reyes','Fwd: the signed contract','mu4e-forwarded-face'));
  L.push(row('D','2026-06-11','(draft)','Notes to finish later','mu4e-draft-face'));
  L.push(row('T','2026-06-10','spam@nowhere','You have won a prize','mu4e-trashed-face'));
  L.push(row('','2026-06-09','Erin (cc)','thread you follow','mu4e-related-face'));
  L.push(row('!','2026-06-08','Frank Diaz','budget needs sign-off','mu4e-flagged-face'));
  L.push('');
  // a message view below the list
  L.push(os(a,'mu4e-header-key-face','From:')+'    '+os(a,'mu4e-contact-face','Christine Park &lt;christine@example.com&gt;'));
  L.push(os(a,'mu4e-header-key-face','To:')+'      '+os(a,'mu4e-special-header-value-face','craig, dev-list@gnu.org'));
  L.push(os(a,'mu4e-header-key-face','Subject:')+' '+os(a,'mu4e-header-value-face','Re: quarterly numbers'));
  L.push('');
  L.push('  Body with a '+os(a,'mu4e-highlight-face','search hit')+', a link '+os(a,'mu4e-url-number-face','[1]')+' '+os(a,'mu4e-link-face','https://example.com')+', and a '+os(a,'mu4e-region-code','code region')+'.');
  L.push('  '+os(a,'mu4e-system-face','*** mu: 340 messages indexed ***'));
  L.push('  '+os(a,'mu4e-footer-face','-- Sent with mu4e'));
  L.push('');
  L.push(os(a,'mu4e-compose-separator-face','--text follows this line--'));
  return previewLines(L);}
function renderGnusPreview(){const a='gnus',L=[];
  // mu4e renders the open message with gnus, so this is the article view:
  // a header block, a body with inline emphasis and a button, then a quoted
  // reply chain (one cite face per nesting level) and the signature.
  L.push(os(a,'gnus-header-name','From: ')+os(a,'gnus-header-from','Christine Park &lt;christine@example.com&gt;'));
  L.push(os(a,'gnus-header-name','To: ')+os(a,'gnus-header-content','craig@cjennings.net'));
  L.push(os(a,'gnus-header-name','Newsgroups: ')+os(a,'gnus-header-newsgroups','gnu.emacs.help'));
  L.push(os(a,'gnus-header-name','Subject: ')+os(a,'gnus-header-subject','Re: quarterly numbers'));
  L.push(os(a,'gnus-header-name','Date: ')+os(a,'gnus-header-content','Sat, 14 Jun 2026 09:12:04 -0500'));
  L.push('');
  L.push('Thanks for the draft. The '+os(a,'gnus-emphasis-bold','revenue line')+' is '+os(a,'gnus-emphasis-italic','close')+', but the '+os(a,'gnus-emphasis-underline','footnote')+' is '+os(a,'gnus-emphasis-strikethru','wrong')+' '+os(a,'gnus-emphasis-highlight-words','FIXME')+'.');
  L.push('See the worksheet: '+os(a,'gnus-button','[https://example.com/q2]'));
  L.push('');
  L.push(os(a,'gnus-cite-attribution','On Fri, Bob Lin wrote:'));
  L.push(os(a,'gnus-cite-1','> The Q2 totals are ready for review.'));
  L.push(os(a,'gnus-cite-2','>> Did the Segpay refund post yet?'));
  L.push(os(a,'gnus-cite-3','>>> Yes, it cleared on the 5th.'));
  L.push(os(a,'gnus-cite-4','>>>> Good, then we are square.'));
  L.push(os(a,'gnus-cite-5','>>>>> earlier reply, level 5'));
  L.push(os(a,'gnus-cite-6','>>>>>> level 6'));
  L.push(os(a,'gnus-cite-7','>>>>>>> level 7'));
  L.push(os(a,'gnus-cite-8','>>>>>>>> level 8'));
  L.push(os(a,'gnus-cite-9','>>>>>>>>> level 9'));
  L.push(os(a,'gnus-cite-10','>>>>>>>>>> level 10'));
  L.push(os(a,'gnus-cite-11','>>>>>>>>>>> level 11'));
  L.push('');
  L.push(os(a,'gnus-signature','-- '));
  L.push(os(a,'gnus-signature','Christine Park, Finance'));
  return previewLines(L);}
function renderOrgFacesPreview(){const a='org-faces',L=[];
  L.push('Agenda header row -- one face per keyword and priority (this config, not built-in org):');
  L.push('');
  L.push(os(a,'org-faces-todo','TODO')+'      Draft the spec            '+os(a,'org-faces-priority-a','[#A]'));
  L.push(os(a,'org-faces-project','PROJECT')+'   Theme studio overhaul     '+os(a,'org-faces-priority-b','[#B]'));
  L.push(os(a,'org-faces-doing','DOING')+'     Wire the faces            '+os(a,'org-faces-priority-c','[#C]'));
  L.push(os(a,'org-faces-waiting','WAITING')+'   On review                 '+os(a,'org-faces-priority-d','[#D]'));
  L.push(os(a,'org-faces-verify','VERIFY')+'    Confirm the round-trip');
  L.push(os(a,'org-faces-stalled','STALLED')+'   Blocked on upstream');
  L.push(os(a,'org-faces-delegated','DELEGATED')+' Handed to Kostya');
  L.push(os(a,'org-faces-failed','FAILED')+'    Could not reproduce');
  L.push(os(a,'org-faces-done','DONE')+'      Shipped the module');
  L.push(os(a,'org-faces-cancelled','CANCELLED')+' Dropped the approach');
  L.push('');
  L.push('Unfocused (auto-dim) -- the -dim variants auto-dim remaps onto in non-selected windows:');
  L.push('');
  L.push(os(a,'org-faces-todo-dim','TODO')+'      Draft the spec            '+os(a,'org-faces-priority-a-dim','[#A]'));
  L.push(os(a,'org-faces-project-dim','PROJECT')+'   Theme studio overhaul     '+os(a,'org-faces-priority-b-dim','[#B]'));
  L.push(os(a,'org-faces-doing-dim','DOING')+'     Wire the faces            '+os(a,'org-faces-priority-c-dim','[#C]'));
  L.push(os(a,'org-faces-waiting-dim','WAITING')+'   On review                 '+os(a,'org-faces-priority-d-dim','[#D]'));
  L.push(os(a,'org-faces-verify-dim','VERIFY')+'    Confirm the round-trip');
  L.push(os(a,'org-faces-stalled-dim','STALLED')+'   Blocked on upstream');
  L.push(os(a,'org-faces-delegated-dim','DELEGATED')+' Handed to Kostya');
  L.push(os(a,'org-faces-failed-dim','FAILED')+'    Could not reproduce');
  L.push(os(a,'org-faces-done-dim','DONE')+'      Shipped the module');
  L.push(os(a,'org-faces-cancelled-dim','CANCELLED')+' Dropped the approach');
  return previewLines(L);}
function renderLspPreview(){const a='lsp-mode',L=[];
  L.push(os(a,'lsp-signature-face','process(')+os(a,'lsp-signature-highlight-function-argument','items: list')+os(a,'lsp-signature-face',') -> None'));
  L.push(os(a,'lsp-signature-posframe',' docs: iterate over items and process each one '));
  L.push('');
  L.push('def process(items):');
  L.push('    n = len(items)'+os(a,'lsp-inlay-hint-type-face',': int'));
  L.push('    handle('+os(a,'lsp-inlay-hint-parameter-face','arg:')+'n)'+os(a,'lsp-inlay-hint-face','   # hint'));
  L.push('    '+os(a,'lsp-face-highlight-read','value')+' = '+os(a,'lsp-face-highlight-write','value')+' + '+os(a,'lsp-face-highlight-textual','value'));
  L.push('    rename '+os(a,'lsp-face-rename','oldName')+' to '+os(a,'lsp-rename-placeholder-face','newName'));
  L.push('    getName()   '+os(a,'lsp-details-face','str   the cached getter'));
  L.push('');
  L.push(os(a,'lsp-installation-buffer-face','Installing pyright...')+'   '+os(a,'lsp-installation-finished-buffer-face','done.'));
  return previewLines(L);}
function renderGitGutterPreview(){const a='git-gutter',L=[];
  L.push(os(a,'git-gutter:added','+')+os(a,'git-gutter:separator','|')+' added line of code');
  L.push(os(a,'git-gutter:modified','~')+os(a,'git-gutter:separator','|')+' modified line of code');
  L.push(os(a,'git-gutter:deleted','_')+os(a,'git-gutter:separator','|')+' (deleted lines marker)');
  L.push(os(a,'git-gutter:unchanged',' ')+os(a,'git-gutter:separator','|')+' '+os(a,'git-gutter:unchanged','unchanged line of code'));
  return previewLines(L);}
function renderEatPreview(){const a='eat',L=[],c=(f,t)=>os(a,'eat-term-color-'+f,t),x=(f,t)=>os(a,'eat-term-'+f,t),an=(g,t)=>os(a,'eat-shell-prompt-annotation-'+g,t);
  const p=g=>an(g,g==='success'?'✔':g==='failure'?'✘':'…')+' ~/projects/app $ ';
  // 1. directory listing -- the widest palette block (dircolors)
  L.push(p('success')+'eza -la --color');
  L.push('drwxr-xr-x     - 14:02  '+c('blue','.git/'));
  L.push('.rw-r--r--   120 09:11  .gitignore');
  L.push('drwxr-xr-x     - 14:02  '+c('blue','src/'));
  L.push('drwxr-xr-x     - 13:48  '+c('blue','tests/'));
  L.push('.rwxr-xr-x  2.1k 14:00  '+c('bright-green','run.sh'));
  L.push('lrwxr-xr-x     - 14:01  '+c('cyan','latest')+' -> '+c('blue','v2.1/'));
  L.push('.rw-r--r--  4.5M 22:30  '+c('red','backup.tar.gz'));
  L.push('.rw-r--r--   88k 18:05  '+c('magenta','logo.png'));
  L.push('.rw-r--r--  3.2k 14:02  README.md');
  L.push('');
  // 2. git status -- staged green, unstaged/untracked red
  L.push(p('success')+'git status -sb');
  L.push(c('bright-cyan','## main...origin/main [ahead 2]'));
  L.push(c('green','A  src/eat-preview.js'));
  L.push(c('green','A  src/cache.el'));
  L.push(c('green','M  README.md'));
  L.push(c('red',' M init.el'));
  L.push(c('red',' M modules/term-config.el'));
  L.push(c('red',' D modules/old-vterm.el'));
  L.push(c('red','?? docs/design/eat.org'));
  L.push(c('red','?? scratch.txt'));
  L.push('');
  // 3. git log --decorate -- yellow hashes, colored refs, a merge
  L.push(p('success')+'git log --oneline --graph --decorate');
  L.push(c('bright-black','* ')+c('yellow','a1b2c3d')+' '+c('bright-cyan','(HEAD -> ')+c('bright-green','main')+c('bright-cyan',')')+' richer eat preview blocks');
  L.push(c('bright-black','* ')+c('yellow','9f8e7d6')+' '+c('bright-yellow','(tag: v2.1, ')+c('bright-red','origin/main')+c('bright-yellow',')')+' lowercase the labels');
  L.push(c('bright-black','*   ')+c('yellow','3c4d5e6')+' Merge branch '+c('green',"'eat-faces'"));
  L.push(c('bright-black','|\\  '));
  L.push(c('bright-black','| * ')+c('yellow','7a8b9c0')+' expose eat faces to studio');
  L.push(c('bright-black','| * ')+c('yellow','1d2e3f4')+' add eat-term-color docstrings');
  L.push(c('bright-black','|/  '));
  L.push(c('bright-black','* ')+c('yellow','5f6a7b8')+' default video player to mpv');
  L.push(c('bright-black','* ')+c('yellow','2c3d4e5')+' calendar-sync robustness fixes');
  L.push('');
  // 4. test run -- pass green, skip yellow, fail red, bold summary, faint detail
  L.push(p('failure')+'make test');
  L.push(c('green','✔ PASS')+'  term-toggle      '+x('faint','(19 tests)'));
  L.push(c('green','✔ PASS')+'  ai-term          '+x('faint','(158 tests)'));
  L.push(c('green','✔ PASS')+'  calendar-sync    '+x('faint','(575 tests)'));
  L.push(c('green','✔ PASS')+'  dashboard        '+x('faint','(18 tests)'));
  L.push(c('yellow','⚠ SKIP')+'  network-sync     '+x('faint','(2 tests, offline)'));
  L.push(c('green','✔ PASS')+'  transcription    '+x('faint','(44 tests)'));
  L.push(c('red','✘ FAIL')+'  org-roam-refile  '+x('faint','(1 test)'));
  L.push('        '+x('italic','expected 3 refile targets, got 0'));
  L.push(x('bold','Ran 817 tests, 815 passed, ')+c('yellow','1 skipped, ')+c('red','1 failed')+'   '+x('faint','0.84s'));
  L.push('');
  // swatch reference key, below the realistic blocks
  L.push(x('faint','palette')+'  '+c('black','■')+c('red','■')+c('green','■')+c('yellow','■')+c('blue','■')+c('magenta','■')+c('cyan','■')+c('white','■')+'  '+c('bright-black','■')+c('bright-red','■')+c('bright-green','■')+c('bright-yellow','■')+c('bright-blue','■')+c('bright-magenta','■')+c('bright-cyan','■')+c('bright-white','■'));
  L.push(x('faint','attrs')+'    '+x('bold','bold')+'  '+x('faint','faint')+'  '+x('italic','italic')+'  '+x('slow-blink','slow-blink')+'  '+x('fast-blink','fast-blink'));
  L.push(x('faint','prompt')+'   '+an('success','✔ ok')+'  '+an('running','… running')+'  '+an('failure','✘ failed'));
  return previewLines(L);}
function renderFlycheckPreview(){const a='flycheck',L=[];
  L.push(os(a,'flycheck-fringe-error','E')+os(a,'flycheck-fringe-warning','W')+os(a,'flycheck-fringe-info','I')+'  x = '+os(a,'flycheck-error','undefined_name')+'('+os(a,'flycheck-warning','unused_arg')+')  '+os(a,'flycheck-info','# note'));
  L.push('       '+os(a,'flycheck-error-delimiter','[')+os(a,'flycheck-delimited-error','err')+os(a,'flycheck-error-delimiter',']'));
  L.push('');
  L.push(os(a,'flycheck-error-list-checker-name','pyright')+'   '+os(a,'flycheck-verify-select-checker','(selected checker)'));
  L.push(os(a,'flycheck-error-list-filename','main.py')+':'+os(a,'flycheck-error-list-line-number','12')+':'+os(a,'flycheck-error-list-column-number','4')+'  '+os(a,'flycheck-error-list-error','error')+'    '+os(a,'flycheck-error-list-error-message','undefined name x')+'  '+os(a,'flycheck-error-list-id','[E0602]'));
  L.push(os(a,'flycheck-error-list-filename','main.py')+':'+os(a,'flycheck-error-list-line-number','18')+':'+os(a,'flycheck-error-list-column-number','1')+'  '+os(a,'flycheck-error-list-warning','warning')+'  '+os(a,'flycheck-error-list-error-message','unused import')+'  '+os(a,'flycheck-error-list-id-with-explainer','[W0611?]'));
  L.push(os(a,'flycheck-error-list-highlight','main.py:20    '+os(a,'flycheck-error-list-info','info')+'     highlighted row'));
  return previewLines(L);}
function renderDiredPreview(){const a='dired',L=[];
  L.push(os(a,'dired-header','/home/craig/code:'));
  L.push('  '+os(a,'dired-perm-write','drwxr-xr-x')+'  craig  4096  '+os(a,'dired-directory','src/'));
  L.push('  -rw-r--r--  craig   120  notes.org');
  L.push('  '+os(a,'dired-perm-write','lrwxrwxrwx')+'  craig    18  '+os(a,'dired-symlink','latest -> v2.1'));
  L.push('  lrwxrwxrwx  craig    --  '+os(a,'dired-broken-symlink','dead -> gone'));
  L.push(os(a,'dired-flagged','D')+' -rw-r--r--  craig    40  deleteme.tmp');
  L.push(os(a,'dired-mark','*')+' '+os(a,'dired-marked','-rw-r--r--  craig   210  marked.txt'));
  L.push('  -rw-r--r--  craig     0  '+os(a,'dired-ignored','.gitignore'));
  L.push('  '+os(a,'dired-set-id','-rwsr-xr-x')+'  root    900  setuid.bin');
  L.push('  '+os(a,'dired-special','prw-r--r--')+'  craig     0  fifo.pipe');
  L.push(os(a,'dired-warning','! disk space low on /home'));
  return previewLines(L);}
// A believable two-pane dirvish: an active directory listing on the left
// (nerd-icon per file type, dir-entry counts / file sizes, the hl-line on the
// selected row, a dimmed backup) and the selected dir's ls-l preview on the
// right.  Faces that don't fit a calm listing (vc, git, subtree, media, proc,
// narrow, emerge) live in a labeled extras strip below so theme coverage stays
// complete.  Glyphs/colors mirror what nerd-icons actually emits per type.
function renderDirvishPreview(){
  const D='dirvish', N='nerd-icons', DR='dired';
  // foreground-only span, so a row background (the hl-line) shows through it
  const fg=(app,face,t)=>`<span style="color:${effFg(pkgEffFg(app,face))}">${t}</span>`;
  const ic=(face,g)=>os(N,face,g);
  const pad=(name,w)=>esc(name)+' '.repeat(Math.max(1,w-name.length));
  const HL=pkgEffBg(D,'dirvish-hl-line')||MAP['bg'];

  // ---- left pane: the active directory ----
  const left=[os(DR,'dired-header','~/code/emacs-wttrin:')];
  for(const [name,cnt,sel] of [['assets','4',true],['examples','1'],['githooks','1'],
                               ['inbox','3'],['scripts','1'],['tests','71']]){
    if(sel) left.push(`<span style="background:${HL}">`+fg(N,'nerd-icons-yellow','')+' '
                      +fg(DR,'dired-directory',pad(name,21))+fg(D,'dirvish-file-size',cnt)+`</span>`);
    else    left.push(ic('nerd-icons-yellow','')+' '+os(DR,'dired-directory',pad(name,21))
                      +os(D,'dirvish-file-size',cnt));
  }
  for(const [face,g,name,size] of [['nerd-icons-lblue','','CLAUDE.md','4.9k'],
       ['nerd-icons-blue','','Eask','518'],['nerd-icons-blue','','LICENSE','34k'],
       ['nerd-icons-dorange','','Makefile','12k'],['nerd-icons-lcyan','','README.org','24k'],
       ['nerd-icons-lgreen','','todo.org','23k'],['nerd-icons-purple','','wttrin.el','69k'],
       ['nerd-icons-dsilver','','wttrin.elc','4.3k']])
    left.push(ic(face,g)+' '+pad(name,21)+os(D,'dirvish-file-size',size));
  left.push(ic('nerd-icons-lgreen','')+' '+os(DR,'dired-ignored',pad('todo.org~',21))
            +os(D,'dirvish-file-size','8.8k'));

  // ---- right pane: ls -l preview of the selected dir ----
  const ll=(size,time,name)=>os(D,'dirvish-file-modes','-rw-r--r--')+' '
    +os(D,'dirvish-file-link-number','1')+' '+os(D,'dirvish-file-user-id','cjennings')+' '
    +os(D,'dirvish-file-group-id','cjennings')+' '+os(D,'dirvish-file-size',size)+' '
    +os(D,'dirvish-file-time',time)+'  '+ic('nerd-icons-orange','\u{F0E2D}')+' '+esc(name);
  const right=[os(DR,'dired-header','assets:'),
    ll('54K','Jun 26 10:53','geolocation.png'),ll('52K','Jun 26 10:53','location-menu.png'),
    ll('3.1K','Apr 10 12:03','made-for-emacs.svg'),ll('346K','Jun 26 10:53','wttrin.png')];

  // ---- extras: remaining dirvish faces, kept for theme coverage ----
  const ex=[
    os(D,'dirvish-inactive','inactive pane')+'   '+os(D,'dirvish-hl-line-inactive',' inactive current line ')+'   '+os(D,'dirvish-free-space','[free 24G]'),
    'vc  '+os(D,'dirvish-vc-added-state','A')+os(D,'dirvish-vc-edited-state','M')+os(D,'dirvish-vc-removed-state','D')+os(D,'dirvish-vc-conflict-state','C')+os(D,'dirvish-vc-locked-state','L')+os(D,'dirvish-vc-missing-state','!')+os(D,'dirvish-vc-needs-merge-face','m')+os(D,'dirvish-vc-needs-update-state','u')+os(D,'dirvish-vc-unregistered-face','?')+'   git '+os(D,'dirvish-git-commit-message-face','feat: enlarge the picker'),
    'subtree '+os(D,'dirvish-collapse-dir-face','src')+os(D,'dirvish-subtree-state','+')+os(D,'dirvish-subtree-guide',' | ')+os(D,'dirvish-collapse-empty-dir-face','empty/')+' '+os(D,'dirvish-collapse-file-face','file.txt')+'   inode '+os(D,'dirvish-file-inode-number','1048576')+' dev '+os(D,'dirvish-file-device-number','8,1'),
    'media '+os(D,'dirvish-media-info-heading','Media')+' '+os(D,'dirvish-media-info-property-key','Dimensions:')+' 1920x1080   proc '+os(D,'dirvish-proc-running','running')+'/'+os(D,'dirvish-proc-finished','finished')+'/'+os(D,'dirvish-proc-failed','failed'),
    'narrow '+os(D,'dirvish-narrow-match-face-0','m0')+' '+os(D,'dirvish-narrow-match-face-1','m1')+' '+os(D,'dirvish-narrow-match-face-2','m2')+' '+os(D,'dirvish-narrow-match-face-3','m3')+os(D,'dirvish-narrow-split',' | ')+os(D,'dirvish-emerge-group-title','Group: images')];

  const col=(lines)=>`<div style="white-space:pre">${lines.join('\n')}</div>`;
  return `<div style="padding:12px 16px;font:12pt/1.7 ${PREVIEW_FONT}">`
    +`<div style="display:flex;gap:2.5em">${col(left)}${col(right)}</div>`
    +`<div style="margin-top:1.2em;opacity:0.9">${col(ex)}</div></div>`;}
function renderCalibredbPreview(){const a='calibredb',L=[];
  L.push(os(a,'calibredb-search-header-library-name-face','Calibre')+'  '+os(a,'calibredb-search-header-library-path-face','~/books')+'  '+os(a,'calibredb-search-header-total-face','412 books')+'  '+os(a,'calibredb-search-header-filter-face','tag:scifi')+'  '+os(a,'calibredb-search-header-sort-face','sort:date')+'  '+os(a,'calibredb-search-header-highlight-face','[*]'));
  L.push('');
  L.push(os(a,'calibredb-id-face','1')+'  '+os(a,'calibredb-title-face','Dune')+'  '+os(a,'calibredb-author-face','Herbert')+'  '+os(a,'calibredb-format-face','EPUB')+'  '+os(a,'calibredb-size-face','2.1M')+'  '+os(a,'calibredb-tag-face',':scifi:')+'  '+os(a,'calibredb-date-face','2026-06-08'));
  L.push(os(a,'calibredb-mark-face','*')+os(a,'calibredb-id-face','2')+'  '+os(a,'calibredb-title-face','Foundation')+'  '+os(a,'calibredb-author-face','Asimov')+'  '+os(a,'calibredb-series-face','[Foundation #1]')+'  '+os(a,'calibredb-publisher-face','Bantam')+'  '+os(a,'calibredb-pubdate-face','1951'));
  L.push('');
  L.push(os(a,'calibredb-title-detailed-view-face','Foundation (detailed)')+'   '+os(a,'calibredb-language-face','eng')+'  '+os(a,'calibredb-favorite-face','* fav')+'  '+os(a,'calibredb-archive-face','archived'));
  L.push(os(a,'calibredb-ids-face','isbn:0553293354')+'  '+os(a,'calibredb-file-face','foundation.epub')+'  '+os(a,'calibredb-comment-face','A classic of the genre.'));
  L.push(os(a,'calibredb-edit-annotation-header-title-face','Annotations')+'  '+os(a,'calibredb-highlight-face','highlighted passage')+'  '+os(a,'calibredb-current-page-button-face','[page 42]')+'  '+os(a,'calibredb-mouse-face','hover row'));
  return previewLines(L);}
function renderErcPreview(){const a='erc',L=[];
  L.push(os(a,'erc-header-line',' #emacs on Libera.Chat  18 users '));
  L.push(os(a,'erc-timestamp-face','[10:24]')+' '+os(a,'erc-notice-face','*** alice has joined #emacs'));
  L.push(os(a,'erc-timestamp-face','[10:25]')+' &lt;'+os(a,'erc-my-nick-prefix-face','@')+os(a,'erc-my-nick-face','craig')+'&gt; '+os(a,'erc-input-face','hello everyone'));
  L.push(os(a,'erc-timestamp-face','[10:25]')+' &lt;'+os(a,'erc-nick-prefix-face','+')+os(a,'erc-nick-default-face','bob')+'&gt; '+os(a,'erc-default-face','hi craig, see ')+os(a,'erc-button','this link')+os(a,'erc-default-face',' cc ')+os(a,'erc-button-nick-default-face','@alice'));
  L.push(os(a,'erc-timestamp-face','[10:26]')+' '+os(a,'erc-action-face','* craig waves')+'   '+os(a,'erc-keyword-face','emacs')+' '+os(a,'erc-pal-face','&lt;friend&gt;')+' '+os(a,'erc-fool-face','&lt;troll&gt;')+' '+os(a,'erc-dangerous-host-face','&lt;bad@host&gt;'));
  L.push(os(a,'erc-timestamp-face','[10:27]')+' '+os(a,'erc-direct-msg-face','(DM)')+' &lt;'+os(a,'erc-nick-msg-face','bob')+'&gt; psst   '+os(a,'erc-current-nick-face','craig')+'   '+os(a,'erc-information','-info-'));
  L.push(os(a,'erc-error-face','*** ERROR: connection reset'));
  L.push(os(a,'erc-command-indicator-face','/help')+'   '+os(a,'erc-bold-face','bold')+' '+os(a,'erc-italic-face','italic')+' '+os(a,'erc-underline-face','underline')+' '+os(a,'erc-inverse-face','inverse')+' '+os(a,'erc-spoiler-face','spoiler'));
  L.push(os(a,'erc-keep-place-indicator-arrow','&gt;')+os(a,'erc-keep-place-indicator-line',' ---- last read ---- ')+os(a,'erc-fill-wrap-merge-indicator-face','+'));
  L.push(os(a,'erc-prompt-face','craig&gt;')+' '+os(a,'erc-input-face','type a message...'));
  return previewLines(L);}
function renderOrgdrillPreview(){const a='org-drill',L=[];
  L.push('Q: The capital of France is '+os(a,'org-drill-hidden-cloze-face','[...]')+'.');
  L.push('A: The capital of France is '+os(a,'org-drill-visible-cloze-face','Paris')+'.');
  L.push('   '+os(a,'org-drill-visible-cloze-hint-face','hint: P____'));
  return previewLines(L);}
function renderOrgnoterPreview(){const a='org-noter',L=[];
  L.push('org-noter   paper.pdf');
  L.push('  page 1   '+os(a,'org-noter-notes-exist-face','[notes]'));
  L.push('  page 2   '+os(a,'org-noter-no-notes-exist-face','[no notes]'));
  return previewLines(L);}
function renderSignelPreview(){const a='signel',L=[];
  L.push(os(a,'signel-timestamp-face','[10:24]')+' '+os(a,'signel-my-msg-face','Me: hey, are we still on for tonight?'));
  L.push(os(a,'signel-timestamp-face','[10:25]')+' '+os(a,'signel-other-msg-face','Christine: yes! see you at 7'));
  L.push(os(a,'signel-error-face','(failed to send -- retrying)'));
  return previewLines(L);}
function renderPearlPreview(){const a='pearl',L=[];
  L.push(os(a,'pearl-preamble-summary','PEARL-42  Fix the broken picker'));
  L.push('State: '+os(a,'pearl-modified-local','In Progress')+'   Priority: '+os(a,'pearl-modified-highlight','High')+'   Estimate: '+os(a,'pearl-modified-unknown','?'));
  L.push('  '+os(a,'pearl-editable-comment','&gt; add a comment (editable)'));
  L.push('  '+os(a,'pearl-readonly-comment','&gt; created by automation (read-only)'));
  return previewLines(L);}
function renderShrPreview(){const a='shr',L=[];
  L.push(os(a,'shr-text','shr renders nov (EPUB), eww (web), elfeed, and HTML mail.'));
  L.push('');
  L.push(os(a,'shr-h1','Chapter One: The Beginning'));
  L.push(os(a,'shr-h2','A Section Heading'));
  L.push(os(a,'shr-h3','A subsection')+'   '+os(a,'shr-h4','h4')+' / '+os(a,'shr-h5','h5')+' / '+os(a,'shr-h6','h6'));
  L.push(os(a,'shr-text','Body text flows in shr-text, with a ')+os(a,'shr-link','hyperlink')+os(a,'shr-text',' and a ')+os(a,'shr-selected-link','focused link')+os(a,'shr-text',','));
  L.push(os(a,'shr-text','some ')+os(a,'shr-code','inline_code()')+os(a,'shr-text',', a ')+os(a,'shr-mark','highlighted mark')+os(a,'shr-text',', ')+os(a,'shr-strike-through','struck out')+os(a,'shr-text',', a footnote')+os(a,'shr-sup','[1]')+os(a,'shr-text',','));
  L.push(os(a,'shr-text','an ')+os(a,'shr-abbreviation','HTML')+os(a,'shr-text',' abbreviation, and an ')+os(a,'shr-sliced-image','[image]')+os(a,'shr-text',' slice.'));
  return previewLines(L);}
function renderSlackPreview(){const a='slack',L=[];
  L.push(os(a,'slack-room-info-title-room-name-face','#general')+'  '+os(a,'slack-room-info-title-face','Acme Workspace'));
  L.push(os(a,'slack-room-info-section-title-face','Topic')+'  '+os(a,'slack-room-info-section-label-face','daily standup')+'   '+os(a,'slack-room-unread-face','3 unread'));
  L.push(os(a,'slack-new-message-marker-face','---------------- new messages ----------------'));
  L.push(os(a,'slack-message-output-header','craig  10:24'));
  L.push('  '+os(a,'slack-message-output-text','hey ')+os(a,'slack-message-mention-me-face','@craig')+os(a,'slack-message-output-text',', see ')+os(a,'slack-message-mention-face','@alice')+os(a,'slack-message-output-text',' in ')+os(a,'slack-channel-button-face','#general')+'  '+os(a,'slack-message-mention-keyword-face','urgent'));
  L.push('  '+os(a,'slack-mrkdwn-bold-face','*bold*')+' '+os(a,'slack-mrkdwn-italic-face','_italic_')+' '+os(a,'slack-mrkdwn-code-face','`code`')+' '+os(a,'slack-mrkdwn-strike-face','~strike~'));
  L.push('  '+os(a,'slack-mrkdwn-blockquote-face','&gt; quoted')+'   '+os(a,'slack-mrkdwn-list-face','- item'));
  L.push('  '+os(a,'slack-mrkdwn-code-block-face','``` code block ```'));
  L.push('  '+os(a,'slack-message-output-reaction',':thumbsup: 3')+'  '+os(a,'slack-message-output-reaction-pressed',':heart: 1')+'   '+os(a,'slack-message-deleted-face','(message deleted)'));
  L.push('  '+os(a,'slack-all-thread-buffer-thread-header-face','Thread: 2 replies'));
  L.push(os(a,'slack-attachment-header','Attachment')+'  '+os(a,'slack-attachment-field-title','Field:')+' val  '+os(a,'slack-message-attachment-preview-header-face','Preview')+'  '+os(a,'slack-preview-face','snippet')+os(a,'slack-attachment-pad',' | ')+os(a,'slack-attachment-footer','footer'));
  L.push(os(a,'slack-block-highlight-source-overlay-face',' highlighted source block '));
  L.push('Actions: '+os(a,'slack-message-action-face','Edit')+' '+os(a,'slack-message-action-primary-face','Approve')+' '+os(a,'slack-message-action-danger-face','Delete'));
  L.push('Blocks: '+os(a,'slack-button-block-element-face','[Button]')+os(a,'slack-button-primary-block-element-face','[Primary]')+os(a,'slack-button-danger-block-element-face','[Danger]')+os(a,'slack-select-block-element-face','[Select v]')+os(a,'slack-overflow-block-element-face','[...]')+os(a,'slack-date-picker-block-element-face','[Date]'));
  L.push('Dialog: '+os(a,'slack-dialog-title-face','Title')+'  '+os(a,'slack-dialog-element-label-face','Label')+' '+os(a,'slack-dialog-element-hint-face','(hint)')+' '+os(a,'slack-dialog-element-placeholder-face','placeholder')+' '+os(a,'slack-dialog-element-error-face','error')+'  '+os(a,'slack-dialog-select-element-input-face','[input v]')+' '+os(a,'slack-dialog-submit-button-face','[Submit]')+os(a,'slack-dialog-cancel-button-face','[Cancel]'));
  L.push('Users: '+os(a,'slack-user-active-face','alice (active)')+' '+os(a,'slack-user-dnd-face','bob (dnd)')+'  '+os(a,'slack-profile-image-face','[img]')+' '+os(a,'slack-user-profile-header-face','Profile')+' '+os(a,'slack-user-profile-property-name-face','Title:')+' Dev');
  L.push('Search: '+os(a,'slack-search-result-message-header-face','#general')+' '+os(a,'slack-search-result-message-username-face','craig'));
  L.push('Modeline: '+os(a,'slack-modeline-has-unreads-face','* unreads')+' '+os(a,'slack-modeline-channel-has-unreads-face','#ch')+' '+os(a,'slack-modeline-thread-has-unreads-face','thread'));
  return previewLines(L);}
function renderTelegaPreview(){const a='telega',L=[];
  L.push(os(a,'telega-root-heading','Telegram')+'  '+os(a,'telega-tracking','[tracking]')+'  '+os(a,'telega-unread-unmuted-modeline','5 unread'));
  L.push(os(a,'telega-has-chatbuf-brackets','[')+os(a,'telega-username','Christine')+os(a,'telega-has-chatbuf-brackets',']')+' '+os(a,'telega-user-online-status','online')+'  '+os(a,'telega-unmuted-count','3')+' '+os(a,'telega-mention-count','@2')+os(a,'telega-delim-face',' | ')+os(a,'telega-secret-title','Secret')+' '+os(a,'telega-muted-count','muted'));
  L.push(os(a,'telega-username','Bob')+' '+os(a,'telega-user-non-online-status','last seen recently')+'   '+os(a,'telega-contact-birthdays-today','birthday today')+'   '+os(a,'telega-shadow','shadow')+' '+os(a,'telega-link','link')+' '+os(a,'telega-blue','blue')+' '+os(a,'telega-red','red'));
  L.push('');
  L.push(os(a,'telega-msg-heading','Today'));
  L.push(os(a,'telega-msg-user-title','Christine')+'  '+os(a,'telega-msg-inline-reply','| reply to Bob')+'  '+os(a,'telega-msg-inline-forward','fwd from Carol')+'  '+os(a,'telega-msg-inline-other','via bot'));
  L.push('  '+os(a,'telega-entity-type-bold','bold')+' '+os(a,'telega-entity-type-italic','italic')+' '+os(a,'telega-entity-type-underline','underline')+' '+os(a,'telega-entity-type-strikethrough','strike')+' '+os(a,'telega-entity-type-code','code')+' '+os(a,'telega-entity-type-spoiler','spoiler'));
  L.push('  '+os(a,'telega-entity-type-pre','pre block')+'  '+os(a,'telega-entity-type-blockquote','&gt; quote')+'  '+os(a,'telega-entity-type-mention','@user')+' '+os(a,'telega-entity-type-hashtag','#tag')+' '+os(a,'telega-entity-type-cashtag','$USD')+' '+os(a,'telega-entity-type-botcommand','/start')+' '+os(a,'telega-entity-type-texturl','link'));
  L.push(os(a,'telega-msg-self-title','Me')+'  '+os(a,'telega-reaction',':+1: 2')+' '+os(a,'telega-reaction-chosen',':heart: 1')+' '+os(a,'telega-reaction-paid',':star: 5')+' '+os(a,'telega-reaction-paid-chosen',':star: paid')+'   '+os(a,'telega-msg-deleted','(deleted)')+' '+os(a,'telega-msg-sponsored','Sponsored'));
  L.push('  checklist '+os(a,'telega-checklist-stats-done','2 done')+' / '+os(a,'telega-checklist-stats-todo','3 todo')+'   '+os(a,'telega-highlight-text-face','search hit')+'   '+os(a,'telega-button-highlight','[active btn]'));
  L.push(os(a,'telega-chat-prompt','&gt;')+' '+os(a,'telega-chat-prompt-aux','reply')+' '+os(a,'telega-chat-input-attachment','[photo.jpg]')+'   '+os(a,'telega-topic-button','# Topic')+'   '+os(a,'telega-filter-active','Main')+' '+os(a,'telega-filter-button-active','[Unread]')+os(a,'telega-filter-button-inactive','[All]'));
  L.push('Buttons '+os(a,'telega-box-button','[box]')+os(a,'telega-box-button-active','[on]')+os(a,'telega-box-button-default-active','[def]')+os(a,'telega-box-button-default-passive','[def-]')+os(a,'telega-box-button-primary-active','[pri]')+os(a,'telega-box-button-primary-passive','[pri-]')+os(a,'telega-box-button-success-active','[ok]')+os(a,'telega-box-button-success-passive','[ok-]'));
  L.push('        '+os(a,'telega-box-button-danger-active','[del]')+os(a,'telega-box-button-danger-passive','[del-]')+os(a,'telega-box-button-ui-active','[ui]')+os(a,'telega-box-button-ui-passive','[ui-]')+os(a,'telega-box-button2-active','[b2]')+os(a,'telega-box-button2-passive','[b2-]')+os(a,'telega-box-button2-white-foreground','[b2w]'));
  L.push('Describe '+os(a,'telega-describe-section-title','Section')+' '+os(a,'telega-describe-subsection-title','Sub')+' '+os(a,'telega-describe-item-title','Item:')+'   enckey '+os(a,'telega-enckey-00','00')+os(a,'telega-enckey-01','01')+os(a,'telega-enckey-10','10')+os(a,'telega-enckey-11','11'));
  L.push('Palette '+os(a,'telega-palette-builtin-blue','blue')+' '+os(a,'telega-palette-builtin-green','green')+' '+os(a,'telega-palette-builtin-orange','orange')+' '+os(a,'telega-palette-builtin-purple','purple'));
  L.push(os(a,'telega-link-preview-sitename','example.com')+'  '+os(a,'telega-link-preview-title','Link preview title'));
  L.push('Webpage '+os(a,'telega-webpage-title','Title')+' '+os(a,'telega-webpage-subtitle','Subtitle')+' '+os(a,'telega-webpage-header','Header')+' '+os(a,'telega-webpage-subheader','Subheader')+' '+os(a,'telega-webpage-outline','outline')+' '+os(a,'telega-webpage-fixed','fixed')+' '+os(a,'telega-webpage-preformatted','pre')+' '+os(a,'telega-webpage-marked','marked')+' '+os(a,'telega-webpage-strike-through','strike')+' '+os(a,'telega-webpage-chat-link','chat-link'));
  return previewLines(L);}
function genericPreview(app){let h='<div style="padding:10px 14px;font:12pt/1.8 '+PREVIEW_FONT+'">';for(const [face,label] of APPS[app].faces)h+=`<div data-face="${face}" style="${ofs(app,face)}">${esc(label)}</div>`;return h+'</div>';}
// Bespoke split preview: a focused window beside its auto-dimmed twin, both
// showing the language selected at the top of the page (kept in sync via the
// langsel onchange, which re-runs buildPkgPreview).  The left pane carries the
// real per-token syntax colors; the right pane shows what auto-dim does -- every
// default/font-lock face remaps to the single `auto-dim-other-buffers' face, so
// the same code collapses to one faded foreground on the dim background.  The
// trailing row demonstrates `auto-dim-other-buffers-hide' (org hidden text whose
// foreground matches the background, so it vanishes in a dimmed window).
function renderAutodimPreview(){
  const a='auto-dim-other-buffers';
  const langsel=document.getElementById('langsel');
  const lang=(langsel&&langsel.value)||Object.keys(SAMPLES)[0];
  const lines=(SAMPLES[lang]||[]).slice(0,9);
  let lit='';
  for(const line of lines){
    if(!line.length){lit+='\n';continue;}
    for(const [k,t] of line)lit+=`<span data-k="${k}" style="${syntaxStyle(k)}">${esc(t)}</span>`;
    lit+='\n';}
  const dimFg=effFg(pkgEffFg(a,'auto-dim-other-buffers')),dimBg=pkgEffBg(a,'auto-dim-other-buffers')||'#000000';
  let dim='';
  for(const line of lines){
    if(!line.length){dim+='\n';continue;}
    for(const [,t] of line)dim+=esc(t);
    dim+='\n';}
  const hideFg=effFg(pkgEffFg(a,'auto-dim-other-buffers-hide')),hideBg=pkgEffBg(a,'auto-dim-other-buffers-hide')||dimBg;
  const foldText='··· folded body (hidden when dimmed) ···';
  const accent=uf('cursor').bg||'#67809c';
  const pane=(label,body,bg,focused)=>
    `<div style="flex:1;min-width:20ch;border:${focused?'2px solid '+accent:'1px solid #2a2a2a'};border-radius:4px;overflow:hidden">`
    +`<div style="text-align:center;font:bold 10pt ${PREVIEW_FONT};padding:4px;color:${focused?'#cdced1':'#8a8a8a'};background:${focused?'#1a1a1a':'#0a0a0a'};border-bottom:1px solid #2a2a2a">${label}</div>`
    +`<div style="padding:10px 12px;font:12pt/1.6 ${PREVIEW_FONT};white-space:pre;background:${bg}">${body}</div></div>`;
  const litBody=lit+'\n'+`<span style="color:#5e6770">${esc(foldText)}</span>`;
  const dimBody=`<span data-face="auto-dim-other-buffers" style="color:${dimFg}">${dim}</span>\n`
    +`<span data-face="auto-dim-other-buffers-hide" style="color:${hideFg};background:${hideBg}">${esc(foldText)}</span>`;
  return `<div style="display:flex;gap:12px;padding:12px 16px;background:${MAP['bg']}">`
    +pane('normal',litBody,MAP['bg'],true)
    +pane('auto-dim',dimBody,dimBg,false)
    +`</div>`;
}
function renderMarkdownPreview(){const a='markdown-mode',L=[];
  const dl='markdown-header-delimiter-face',mk='markdown-markup-face';
  L.push(os(a,mk,'---'));
  L.push(os(a,'markdown-metadata-key-face','title:')+' '+os(a,'markdown-metadata-value-face','Project Name'));
  L.push(os(a,'markdown-metadata-key-face','version:')+' '+os(a,'markdown-metadata-value-face','1.2.0'));
  L.push(os(a,mk,'---'));
  L.push('');
  L.push(os(a,dl,'#')+' '+os(a,'markdown-header-face-1','Project Name'));
  L.push('');
  L.push(os(a,'markdown-comment-face','&lt;!-- a one-line tagline --&gt;'));
  L.push('A library for '+os(a,'markdown-bold-face','**doing things**')+' and '+os(a,'markdown-italic-face','*other things*')+'.');
  L.push('');
  L.push(os(a,dl,'##')+' '+os(a,'markdown-header-face-2','Installation'));
  L.push('');
  L.push('Run '+os(a,'markdown-inline-code-face','`npm install project`')+' to get started.');
  L.push('');
  L.push(os(a,mk,'```')+os(a,'markdown-language-keyword-face','sh'));
  L.push(os(a,'markdown-pre-face','  git clone https://example.com/project.git'));
  L.push(os(a,'markdown-pre-face','  cd project; make'));
  L.push(os(a,mk,'```'));
  L.push('');
  L.push(os(a,dl,'###')+' '+os(a,'markdown-header-face-3','Usage'));
  L.push('');
  L.push(os(a,'markdown-list-face','- ')+'See the '+os(a,'markdown-link-face','[docs]')+os(a,'markdown-url-face','(https://example.com/docs)')+' for details.');
  L.push(os(a,'markdown-list-face','- ')+'Or browse '+os(a,'markdown-plain-url-face','https://example.com')+' directly.');
  L.push(os(a,'markdown-gfm-checkbox-face','- [x]')+' shipped     '+os(a,'markdown-gfm-checkbox-face','- [ ]')+' planned');
  L.push('');
  L.push(os(a,'markdown-blockquote-face','> A note worth quoting, with a footnote')+os(a,'markdown-footnote-marker-face','[^1]')+os(a,'markdown-blockquote-face','.'));
  L.push('');
  L.push(os(a,'markdown-table-face','| Option | Default |'));
  L.push(os(a,'markdown-table-face','|--------|---------|'));
  L.push(os(a,'markdown-table-face','| debug  | false   |'));
  L.push('');
  L.push(os(a,'markdown-hr-face','---'));
  L.push('');
  L.push(os(a,'markdown-strike-through-face','~~deprecated~~')+'  '+os(a,'markdown-highlight-face','==important==')+'  '+os(a,'markdown-math-face','$E = mc^2$'));
  L.push(os(a,'markdown-html-tag-delimiter-face','&lt;')+os(a,'markdown-html-tag-name-face','kbd')+os(a,'markdown-html-tag-delimiter-face','&gt;')+'Ctrl-C'+os(a,'markdown-html-tag-delimiter-face','&lt;/')+os(a,'markdown-html-tag-name-face','kbd')+os(a,'markdown-html-tag-delimiter-face','&gt;'));
  L.push(os(a,'markdown-footnote-marker-face','[^1]:')+' '+os(a,'markdown-footnote-text-face','the footnote text.'));
  return previewLines(L);}
// nerd-icons gallery grid: the full colored catalog. Every distinct face-bearing
// nerd-icons glyph (APPS['nerd-icons'].gallery, captured by build-nerd-icons-legend.el),
// one row per color face, the rows ordered by hue so families cluster (blues
// together, reds together). Each cell draws the glyph in its face color with the
// icon's nerd-font name beneath. SIZEPT (points, default 14) sizes the glyphs so
// the designer can view the grid at different buffer sizes via the preview-pane
// dropdown; the cell width scales with it. Recoloring a face repaints its swatch
// and every glyph in its row because os() reads the live registry. Falls back to
// the generic preview if the gallery is missing (the bespoke app registers with a
// valid legend, so that path is defensive).
function renderNerdIconsPreview(sizePt){
  const a='nerd-icons',groups=(APPS[a]&&APPS[a].gallery)||[];
  if(!groups.length)return genericPreview(a);
  const pt=sizePt||14,cellW=Math.round(pt*4.6+24);
  let h=`<div class="ni-gallery" style="padding:10px 14px;font:10pt/1.4 ${PREVIEW_FONT}">`;
  for(const g of groups){
    h+='<div class="ni-row" style="margin:0 0 10px;border-top:1px solid #2a2a2a;padding-top:6px">'
      +`<div class="ni-row-head" style="color:#8a8a8a;padding:0 0 5px">`
      +os(a,g.face,'■')+' '+esc(g.face)+' ('+g.glyphs.length+')</div>'
      +'<div class="ni-cells">';
    for(const e of g.glyphs)
      h+=`<span class="ni-cell" style="display:inline-block;width:${cellW}px;text-align:center;vertical-align:top;margin:3px 1px">`
        +`<span style="font-size:${pt}pt;line-height:1.3">`+os(a,g.face,e.glyph)+'</span><br>'
        +`<span style="font-size:7.5pt;color:#9a9a9a;word-break:break-all;line-height:1.2">`+esc(e.name)+'</span>'
        +'</span>';
    h+='</div></div>';
  }
  return h+'</div>';}
