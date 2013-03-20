Main: Main.hs Application.hs Routes.hs MustacheTemplates.hs
	ghc -Wall -fno-warn-name-shadowing Main.hs

Routes.hs: routes
	routeGenerator -r -m Application -n 0 $< > $@

PathHelpers.hs: routes
	routeGenerator -p -n 0 $< > $@

MustacheTemplates.hs: Records.hs view/report.mustache
	mustache2hs -m Records.hs view/report.mustache Report > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main Routes.hs PathHelpers.hs MustacheTemplates.hs
