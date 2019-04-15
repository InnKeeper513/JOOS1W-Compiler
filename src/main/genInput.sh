if [ "$#" -ne 1 ]; then
    echo "Usage: $0 filename.annotated"
fi

cat $1 | grep -v "^-" > input.used
cat input.used | sed "s/ /\n/g" | sort | uniq | grep -E "^[A-Z_][A-Z_]+$" > term
cat input.used | sed "s/ /\n/g" | sort | uniq | grep -v -E "^[A-Z_][A-Z_]+$" > nonterm

wc -l < term > input.cfg
cat term >> input.cfg
wc -l < nonterm >> input.cfg
cat nonterm >> input.cfg
echo "S" >> input.cfg
wc -l < input.used >> input.cfg
cat input.used >> input.cfg

rm input.used term nonterm