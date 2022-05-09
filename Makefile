.DEFAULT_GOAL := serve

update:
	go install github.com/jackyzha0/hugo-obsidian@latest
	@git remote show upstream || (echo "remote 'upstream' not present, setting 'upstream'" && git remote add upstream https://github.com/jackyzha0/quartz.git)
	git fetch upstream
	git log --oneline --decorate --graph ..upstream/hugo
	git checkout -p upstream/hugo -- layouts .github Makefile assets/js assets/styles/base.scss assets/styles/darkmode.scss config.toml data

serve:
	hugo-obsidian -input=content -output=assets/indices -index -root=. && hugo server --enableGitInfo
