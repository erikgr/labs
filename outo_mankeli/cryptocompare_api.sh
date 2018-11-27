#
#
#


CC_API_URL="https://min-api.cryptocompare.com";
CC_MARKETS="CCCAGG";
CC_LIMIT="30" # not used

function cc_api_get {

	local endpoint="$1";
	local getparams="$2";
	curl \
		--get \
		-D /tmp/cryptocompare_headers.txt \
		"${CC_API_URL}${endpoint}?${getparams}";
}


function cc_get_history {

	local resolution="$1";
	local currency_from="$2";
	local currency_to="$3";

	case "${resolution}" in
		"day")
			cc_hist_day "${currency_from}" "${currency_to}";
		;;
		"hour")
			cc_hist_hour "${currency_from}" "${currency_to}";
		;;
		"minute")
			cc_hist_minute "${currency_from}" "${currency_to}";
		;;
	esac
}


function cc_hist_minute {

	local currency_from="$1";
	local currency_to="$2";
	local endpoint="/data/histominute";
	local getparams="";
	local parameters=(
		"fsym=${currency_from}"
		"tsym=${currency_to}"
		"e=${CC_MARKETS}"
		"sign=false"
		"limit=${CC_LIMIT}"
	);
	for parameter in "${parameters[@]}"; do
		getparams+="&${parameter}";
	done
	local data=$(cc_api_get "$endpoint" "$getparams");

	echo "$data" > debug.json

	[[ "Success" = "$(jq -jr .Response < <(echo $data))" ]] && {
		echo "$data";
		return 0;
	} || {
		local msg=$(jq .Message < <(echo "$data"));
		echoerr "${msg}";
		return 1;
	}
}


function cc_hist_hour {

	local currency_from="$1";
	local currency_to="$2";
	local endpoint="/data/histohour";
	local getparams="";
	local parameters=(
		"fsym=${currency_from}"
		"tsym=${currency_to}"
		"e=${CC_MARKETS}"
		"sign=false"
		"limit=${CC_LIMIT}"
	);
	for parameter in "${parameters[@]}"; do
		getparams+="&${parameter}";
	done
	local data=$(cc_api_get "$endpoint" "$getparams");
	[[ "Success" = "$(jq -jr .Response < <(echo $data))" ]] && {
		echo "$data";
		return 0;
	} || {
		local msg=$(jq .Message < <(echo "$data"));
		echoerr "${msg}";
		return 1;
	}
}


function cc_hist_day {

	local currency_from="$1";
	local currency_to="$2";
	local endpoint="/data/histoday";
	local getparams="";
	local parameters=(
		"fsym=${currency_from}"
		"tsym=${currency_to}"
		"e=${CC_MARKETS}"
		"sign=false"
		"limit=${CC_LIMIT}"
	);
	for parameter in "${parameters[@]}"; do
		getparams+="&${parameter}";
	done
	local data=$(cc_api_get "$endpoint" "$getparams");
	[[ "Success" = "$(jq -jr .Response < <(echo $data))" ]] && {
		echo "$data";
		return 0;
	} || {
		local msg=$(jq .Message < <(echo "$data"));
		echoerr "${msg}";
		return 1;
	}
}
