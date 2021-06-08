# Modified from https://www.innoq.com/en/blog/github-actions-automation/

set -eu
echo "Starting bash script"

repo_uri="https://x-access-token:${GITHUB_TOKEN}@github.com/${GITHUB_REPOSITORY}.git"

git config user.name "$GITHUB_ACTOR"
git config user.email "${GITHUB_ACTOR}@bots.github.com"

echo "Adding files"
git add --force .

echo "Committing files"
git commit -m "Updated GitHub pages"
if [ $? -ne 0 ]; then
    echo "Nothing to commit"
    exit 0
fi

echo "Pushing to gh-pages"
git remote set-url origin "$repo_uri"
git push --force origin gh-pages
