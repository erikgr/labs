#
#
#


function atoc {

	local color="";

	case "$1" in
		r)
			color='\033[1;31m';
		;;
		R)
			color='\033[0;31m';
		;;
		g)
			color='\033[1;32m';
		;;
		G)
			color='\033[0;32m';
		;;
		b)
			color='\033[1;34m';
		;;

		B)
			color='\033[0;34m';
		;;
		y)
			color='\033[1;33m';
		;;

		Y)
			color='\033[0;33m';
		;;
	esac

	echo -e "${color}";
}


function rmattr {
	echo -e '\033[0m'
}


function echoerr {
	>&2 cat - <<-!
		$(atoc r)
		>> Error :: ${FUNCNAME[*]} :: $@
		$(rmattr)
	!
}

function echoc {
	atoc "$1";
	shift;
	echo "$@";
	rmattr;
}
