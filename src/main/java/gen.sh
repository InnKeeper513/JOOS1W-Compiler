javac -Xlint:deprecation Jlalr1.java
mv *.class jlalr
java jlalr.Jlalr1 < ../input.cfg > ../resources/output.lr1
