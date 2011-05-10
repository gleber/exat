erl -pa ../../ebin << EOF
ontology:compile ("$1", [include, source]).
halt ().
EOF
