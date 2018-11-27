#
#
#


set -u;


source 'print.sh';
source 'cryptocompare_api.sh';
source 'sqlite.sh';


DBNAME="cryptodb";
TABLENAME="history";
RESOLUTIONS=(
	"day"
	"hour"
	"minute"
)
PAIRS=(
	"BTC/USD"
	"ETH/USD"
	"LTC/USD"
	"IOT/USD"
	"REQ/USD"
);



function create_databases {
	echoc "G" ">> Creating databases";
	do_sql "${DBNAME}" < <(sqlite_create_hist_table "${TABLENAME}") \
		&& return 0 \
		|| return 1;
}


function update_data {

	for resolution in "${RESOLUTIONS[@]}"; do
		for pair in "${PAIRS[@]}"; do

			local currency_from=$(echo "${pair}" | awk -F "/" '{print $1}');
			local currency_to=$(echo "${pair}" | awk -F "/" '{print $2}');

			echoc "G" ">> Updating ${resolution} data for ${currency_from} / ${currency_to}";
			do_sql "${DBNAME}" < <(cc_get_history "${resolution}" "${currency_from}" "${currency_to}" \
						| sqlite_cc_parse_to_insert "${TABLENAME}" "${resolution}" "${currency_from}" "${currency_to}");
			echoc "Y" ">> Done"
			sleep 5;
		done
	done
}


create_databases && update_data;
