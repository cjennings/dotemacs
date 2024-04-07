;;; eradio-config --- Simple Internet Radio Setup -*- lexical-binding: t; -*-
;; author Craig Jennings <c@cjennings.net>

;;; Commentary:

;;; Code:

(use-package eradio
  :bind
  ("C-c r p"     . eradio-play)
  ("C-c r s"     . eradio-stop)
  ("C-c r <SPC>" . eradio-toggle)
  :config
  (setq eradio-player '("mpv" "--no-video" "--no-terminal"))
  (setq eradio-channels
		'(("BAGeL Radio                  (alternative)"  . "https://ais-sa3.cdnstream1.com/2606_128.mp3")
		  ("Blues Music Fan Radio        (blues)"        . "http://ais-sa2.cdnstream1.com/1992_128.mp3")
          ("Blues Radio                  (blues)"        . "http://cast3.radiohost.ovh:8352/")
		  ("Concertzender Baroque        (classical)"    . "http://streams.greenhost.nl:8080/barok")
          ("Groove Salad                 (somafm)"       . "https://somafm.com/groovesalad130.pls")
		  ("Indie Pop Rocks              (somafm)"       . "https://somafm.com/indiepop130.pls")
          ("KDFC Classical               (classical)"    . "http://128.mp3.pls.kdfc.live/")
          ("Radio Caprice Classical Lute (classical)"    . "http://79.120.12.130:8000/lute")
		  ("Radio Swiss Classic German   (classical)"    . "http://stream.srg-ssr.ch/m/rsc_de/mp3_128")
		  ("Radio Caprice Acoustic Blues (blues)"        . "http://79.111.14.76:8000/acousticblues")
		  ("Radio Caprice Delta Blues    (blues)"        . "http://79.120.77.11:8002/deltablues")
		  ("Seven Inch Soul              (somafm)"       . "https://somafm.com/nossl/7soul.pls")
          ("Space Station Soma           (somafm)"       . "https://somafm.com/spacestation.pls")
          ("Suburbs of Goa               (somafm)"       . "https://somafm.com/suburbsofgoa.pls")
          ("Sunday Baroque               (classical)"    . "http://wshu.streamguys.org/wshu-baroque")
          ("Underground 80s              (somafm)"       . "https://somafm.com/u80s256.pls")
          ("Venice Classic Radio         (classical)"    . "https://www.veniceclassicradio.eu/live1/128.m3u")
		  ("WWOZ New Orleans             (jazz/blues)"   . "https://www.wwoz.org/listen/hi"))))

(provide 'eradio-config)
;;; eradio-config.el ends here
