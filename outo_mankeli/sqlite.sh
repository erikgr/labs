#
#
#



function do_sql {
	local dbname="$1";
	sqlite3 "$dbname";
}


function sqlite_create_hist_table {

	local table_name="$1";

	cat - <<-!
		CREATE TABLE IF NOT EXISTS ${table_name} (
			clock integer NOT NULL,
			resolution text NOT NULL,
			currency_from text NOT NULL,
			currency_to text NOT NULL,
			price real NOT NULL,
			PRIMARY KEY (clock, resolution, currency_from, currency_to)
		);
	!
}


function sqlite_cleandb {

	local schema_name="$1";
	local table_name="$2";

	cat - <<-!
		DELETE FROM ${table_name}
			WHERE clock IS NULL
				OR currency_from IS NULL
				OR currency_to IS NULL
				OR price IS NULL;
	!
}

function sqlite_cc_parse_to_insert {

	local table_name="$1";
	local resolution="$2";
	local currency_from="$3";
	local currency_to="$4";
	local parse_cmd=$(cat - <<-!
		.Data[] | "INSERT OR REPLACE INTO ${table_name} (clock, resolution, currency_from, currency_to, price) VALUES (\(.time),'${resolution}','${currency_from}','$currency_to',\(.open));"
		!
	);

	jq -r "${parse_cmd}";
}
