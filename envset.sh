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

	command_path="${GRASP}/bin/${command}"
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
