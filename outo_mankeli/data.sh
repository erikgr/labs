#
#
#





function statfuncs {

	cat  - <<-!
		cov =: 4 : 0
			m=.(+/%#)x
			n=.(+/%#)y
			l=._1+#y
			(+/(x-m)*(y-n))%l
		)

		stddev =: 3 : 0
			a=.(+/%#)y
			%:(+/%#)((y-a)^2)
		)

		corr =: 4 : 0
			m=.x-(+/%#)x
			n=.y-(+/%#)y
			t=.+/(m*n)
			b=.(+/(m^2))*(+/(n^2))
			t%%:b
		)

		eucl_d =: 4 : 0
			%:+/((x-y)^2)
		)

		mavg =: 4 : 0
			x(+/%#)\y
		)
	!
}


function sqlfuncs {

	cat - <<-!
		require'data/sqlite'

		NB. x - resolution
		NB. y - from | to
		sql_history =: 3 : 0
			resolution =. (":>0{y)
			currency_from =. (":>1{y)
			currency_to =. (":>2{y)
			'SELECT clock, price FROM history WHERE resolution="',resolution,'" AND currency_from="',currency_from,'" AND currency_to="',currency_to,'" ORDER BY clock ASC;'
		)

		NB. x - from
		NB. y - to
		sql_history_all =: 4 : 0
			d =. 1{"2>>1{"1 sqlread__db sql_history ('day';x;y)
			h =. 1{"2>>1{"1 sqlread__db sql_history ('hour';x;y)
			m =. 1{"2>>1{"1 sqlread__db sql_history ('minute';x;y)
			>d;h;m
		)

		sql_last_price =: 4 : 0
			'SELECT price FROM history WHERE resolution="minute" AND currency_from="',(":x),'" AND currency_to="',(":y),'" ORDER BY CLOCK DESC LIMIT 1;'
		)

		sql_asset =: 4 : 0
			'select sum(amount) as amount from assets where owner ="',(":x),'" and asset="',(":y),'";'
		)

	!
}


function xformfuncs {

	cat - <<-!
		xform_averages =: 4 : 0
			a =. (>0{x)(mavg"1)y
			b =. (>1{x)(mavg"1)y
			>a;b
		)

		xform_stretch =: 4 : 0
			a =. #x
			b =. #y
			r =. <.b%a
			R =. a|b
			(-R)|.b$,/r($"0)x
		)
	!
}


function classify_funcs {

	cat - <<-!
		NB. x - model
		NB. y - timeseries
		NB. Calculate correlation factor between
		NB. a model timeseries and actual timeseries
		NB. in an attempt to identify interesting events
		NB. in the history.
		classify =: 4 : 0
			M =. (>10<\y) (xform_stretch"0 1 1) x
			c =. (x (corr"0 1 1) M) xform_stretch y
			C =. (0.80 < c) * y
		)
	!
}


# you need to populate the db before
# running me. see mankeli.sh.
function testj {
	cat - <<-!
		require'plot'
		db =: sqlopen_psqlite_ '$(pwd)/cryptodb'

		echo >>1{ sqlread__db ('BTC'sql_last_price'USD')
		echo >>1{ sqlread__db ('ETH'sql_last_price'USD')
		echo >>1{ sqlread__db ('LTC'sql_last_price'USD')
		echo >>1{ sqlread__db ('IOT'sql_last_price'USD')
		echo >>1{ sqlread__db ('REQ'sql_last_price'USD')

		plot ('ETH' sql_history_all 'USD')

	!
}

#
#
ijconsole < <(cat \
		<(sqlfuncs) \
		<(statfuncs) \
		<(xformfuncs) \
		<(classify_funcs) \
		<(testj));


