echo "Repo info:"
curl -X GET -H"authorization: Bearer ${GITHUB_TOKEN}" "https://api.github.com/repos/mortenpi/grasp"

echo "List gh-pages builds:"
curl -X GET -H"authorization: Bearer ${GITHUB_TOKEN}" "https://api.github.com/repos/mortenpi/grasp/pages/builds"

echo "Trigger gh-pages build:"
curl -X POST -H"authorization: Bearer ${GITHUB_TOKEN}" "https://api.github.com/repos/mortenpi/grasp/pages/builds"
