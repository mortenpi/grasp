# Set the GRASP variable to the root of the GRASP source directory
export GRASP="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Set the MPI_TMP directory such that the programs would create a tmp_mpi/
# directory in the current working directory and use that for the temporary
# MPI files.
export MPI_TMP="tmp_mpi"

# Defines the grasp function that can be used to call the different GRASP tools
# on the command line without having to give the full path or making changes to
# $PATH. Requires the $GRASP variable to be set.
#
# If also appropriately calls `mpirun` for the programs that are MPI-enabled
# (ones whose name ends with _mpi). If, in addition, the GRASP_NPROC variable
# is set, then `mpirun` gets the value via the `-n` option. If the variable is
# unset, `mpirun` gets called without the `-n` option, using whatever the
# default happens to be (e.g. the number of tasks in SLURM jobs).
#
# Example:
#
#    $ grasp rcsfgenerate
#    $ GRASP_NPROC=4 grasp rangular_mpi
#
function grasp {
	# Parse out options with getopt. The + in the -o option makes sure that we
	# do not parse options that come after the first positional argument (which
	# we expect to be the user-specified GRASP binary, e.g. rmcdhf).
	VERBOSE=0
	BUILDDIR=""
	options=$(getopt -n grasp -o +hb: -- "$@")
	if ! [ $? -eq 0 ]; then
		>&2 echo "ERROR: Invalid arguments to grasp ($@)"
		return 1
	fi
	eval set -- "$options"
	while true; do
	    case "$1" in
	    -b)
			shift
			echo "user specified buildir $1"
	        BUILDDIR=$1
			if ! [ -d "${GRASP}/${BUILDDIR}" ]; then
				>&2 echo "ERROR: No such build directory \${GRASP}/${BUILDDIR}"
				>&2 echo "  at ${GRASP}/${BUILDDIR}"
				return 1
			fi
	        ;;
	    --)
	        shift
	        break
	        ;;
	    esac
	    shift
	done

	# We're left with just the positional arguments. Note: everything after
	# the first positional argument gets interpreted as non-option in getopt
	# above. So they are still part of $@ and we can pass them on to the GRASP
	# binary.
	local command=$1
	shift

	if [ -z ${GRASP+x} ]; then
		>&2 echo "ERROR: \$GRASP variable is unset. Load GRASP's envset.sh script."
		return 1
	fi

	local mpirun=""
	if [ "${command: -4}" == "_mpi" ]; then
		if [ -z ${GRASP_NPROC+x} ]; then
			# If GRASP_NPROC is unset we run mpirun without argument, allowing
			# it to use its default number of cores.
			mpirun="mpirun"
		else
			mpirun="mpirun -n ${GRASP_NPROC}"
		fi
	fi

	if [ -z "$BUILDDIR" ]; then
		command_path="${GRASP}/bin/${command}"
	else
		>&2 echo "WARNING: Running non-installed GRASP binaries from \${GRASP}/${BUILDDIR}"
		command_path="${GRASP}/${BUILDDIR}/bin/${command}"
	fi
	if ! [ -f "${command_path}" ]; then
		>&2 echo "ERROR: Missing GRASP program ${command}"
		>&2 echo "  Searched at ${command_path}"
		return 1
	fi

	${mpirun} "${command_path}" $@
}

# This enables bash completion for the grasp command. It will simply look at
# the files present in the bin/ directory and proposes them for completion for
# the first argument of the grasp command.
#
# Example:
#
#    $ grasp rci[TAB]
#    rci          rci_mpi
#
function _grasp {
	local binaries current

	# On error conditions we silentely just do not return anything.
	if [ -z ${GRASP+x} ]; then return 1; fi
	if ! [ -d ${GRASP}/bin ]; then return 1; fi

	if (( $COMP_CWORD == 1 )); then
		current="${COMP_WORDS[COMP_CWORD]}"
		binaries=`ls ${GRASP}/bin`
		COMPREPLY=( $(compgen -W "${binaries}" -- ${current}) )
		return 0
	fi
}
complete -F _grasp -o bashdefault -o default grasp

# Rebuilds the whole GRASP. It can be called from anywhere. It will cd into the
# build/ directory and run make install to install all the binaries.
function grasp-make {
	if [ -z ${GRASP+x} ]; then
		>&2 echo "ERROR: \$GRASP variable is unset. Load GRASP's envset.sh script."
		return 1
	fi

	if [ ! -d ${GRASP}/build ]; then
		>&2 echo "ERROR: No build/ directory under \${GRASP}"
		>&2 echo "  \${GRASP}=${GRASP}"
		return 1
	fi

	make -C ${GRASP}/build install
}

# Reloads the GRASP envset.sh script.
function grasp-reload-env {
	if [ -z ${GRASP+x} ]; then
		>&2 echo "ERROR: \$GRASP variable is unset. Load GRASP's envset.sh script."
		return 1
	fi

	if [ ! -f ${GRASP}/envset.sh ]; then
		>&2 echo "ERROR: \${GRASP}/envset.sh is missing."
		>&2 echo "  \${GRASP}=${GRASP}"
		return 1
	fi

	source ${GRASP}/envset.sh
}
