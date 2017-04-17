task foo
	:: param=(Letter: a b c)
	> letter
{
    echo ${param} > ${letter}
}

task bar
	< in=$letter@foo[Letter:*]
{
	echo ${in}
}

