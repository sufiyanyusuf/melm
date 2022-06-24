generate:
	elm make src/Main.elm --output public/app.js --debug

prod:
	elm make src/Main.elm --output public/app.js