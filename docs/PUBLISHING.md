# Publishing on GitHub

This project uses `gh` (GitHub CLI) for repository creation and pushing.

## One-time setup
```
gh auth login
```

## Create the repository
```
gh repo create haskell-agent --public --source . --remote origin
```

Use `--private` instead of `--public` if desired.

## Push the code
```
git add .
git commit -m "Add documentation and polish package metadata"
git push -u origin master
```

Update `haskell-agent.cabal` fields (`homepage`, `bug-reports`, and
`source-repository`) if your GitHub org or repo name differs.
