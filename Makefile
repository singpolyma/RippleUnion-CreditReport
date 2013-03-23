Main: Main.hs Application.hs Routes.hs MustacheTemplates.hs PathHelpers.hs Sqlite3.hs Keyserver.hs Assertion.hs Util.hs
	ghc -Wall -fno-warn-name-shadowing Main.hs

Routes.hs: routes
	routeGenerator -r -m Application -n 2 $< > $@

PathHelpers.hs: routes
	routeGenerator -p -n 2 $< > $@

MustacheTemplates.hs: Records.hs view/report.mustache view/home.mustache
	mustache2hs -m Records.hs view/report.mustache Report view/home.mustache HomeRec > $@

clean:
	find -name '*.o' -o -name '*.hi' | xargs $(RM)
	$(RM) -r dist dist-ghc Main Routes.hs PathHelpers.hs MustacheTemplates.hs
