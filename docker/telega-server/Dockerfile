# telega-server image for modules/telega-config.el.  Build with `make telega-image'.
#
# Upstream's zevlg/telega-server:latest ships tdlib 1.8.66 (the floor the
# current telega package demands) but its telega-server binary fails to start:
# libgdk_pixbuf needs libglycin-2.so.0 and the image doesn't carry it.  Known
# open bug: https://github.com/zevlg/telega.el/issues/596.  This derived image
# adds the one missing package and nothing else.
#
# The base is pinned by digest, not tag, so a rebuild on another machine gets
# the same upstream layers.  libglycin itself is deliberately not version
# pinned: Alpine's repos keep only the current build of a package, so an exact
# `libglycin=X-rN' fails the build the day it rotates, which is worse than one
# layer differing between machines.  When #596 is fixed upstream, point
# `cj/telega-docker-image' back at a registry digest and delete this file.
FROM zevlg/telega-server@sha256:aa7e79c4e9587c84065782df9399653fb1a967078b08ec1ded77738d4c2d70ad
RUN apk add --no-cache libglycin
