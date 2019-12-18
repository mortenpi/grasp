find . -type f -not -path './.git/*' -not -path './grasptest/*' -not -path './make_environment*' -perm /uog+x -exec chmod a-x {} \;
find . -type f -not -path './.git/*' -not -path './grasptest/*' -exec sed --in-place -e :a -e '/^\n*$/{$d;N;};/\n$/ba' {} \;
find . -type f -not -path './.git/*' -not -path './grasptest/*' -exec sed --in-place 's/[[:space:]]\+$//' {} \;
