#!/usr/bin/env bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${DIR}" || { >&2 echo "ERROR: can't enter directory ${DIR}"; exit 1; }

if [ "$1" = "cleanup" ]; then
	rm -vf isodata rcsf.log rcsf.out rcsf.inp rcsfgenerate.log clist.new rwfn.inp
	rm -vf rci.res teststate.*
	exit 0
fi

RNUCLEUS=${GRASP_BINDIR}/rnucleus
RCSFGENERATE=${GRASP_BINDIR}/rcsfgenerate
RWFNESTIMATE=${GRASP_BINDIR}/rwfnestimate
RCI=${GRASP_BINDIR}/rci

if ! [ -f "${RNUCLEUS}" ]; then >&2 echo "ERROR: Missing executable ${RNUCLEUS}"; exit 1; fi
if ! [ -f "${RCSFGENERATE}" ]; then >&2 echo "ERROR: Missing executable ${RCSFGENERATE}"; exit 1; fi
if ! [ -f "${RWFNESTIMATE}" ]; then >&2 echo "ERROR: Missing executable ${RWFNESTIMATE}"; exit 1; fi
if ! [ -f "${RCI}" ]; then >&2 echo "ERROR: Missing executable ${RCI}"; exit 1; fi

echo "Generating a nucleus:"
${RNUCLEUS} <<-EOF || { >&2 echo "ERROR: rnucleus failed with $?"; exit 2; }
	100
	230
	n
	0
	0
	0
	0
EOF
if ! [ -f "isodata" ]; then
	>&2 echo "ERROR: rnucleus didn't generate isodata"
	exit 2
fi

echo "Generating a CSL:"
${RCSFGENERATE} <<-EOF || { >&2 echo "ERROR: rcsfgenerate failed with $?"; exit 2; }
	*
	6
	7s(2,*)

	7s,7p,7d,7f
	0,4
	2
	n
EOF

if ! [ -f "rcsf.out" ]; then
	>&2 echo "ERROR: rcsfgenerate didn't generate rcsf.out"
	exit 2
fi
mv rcsf.out rcsf.inp || exit 2

echo "Generating a orbital file:"
${RWFNESTIMATE} <<-EOF || { >&2 echo "ERROR: rwfnestimate failed with $?"; exit 2; }
	y
	3
	*
EOF
if ! [ -f "rwfn.inp" ]; then
	>&2 echo "ERROR: rwfnestimate didn't generate rwfn.inp"
	exit 2
fi

mv rcsf.inp teststate.c || exit 2
mv rwfn.inp teststate.w || exit 2

# The CSL has: 21 / 27 / 46 CSFs in each block
echo "Running an RCI calculation to generate a mixing file"
${RCI} <<-EOF || { >&2 echo "ERROR: rci failed with $?"; exit 2; }
	y
	teststate
	n
	n
	n
	n
	n
	n
	1-7
	1
	1-46
EOF

exit 0
