Instruction on runnning the GCL parser

Key files: "GCLLexer.fsl", "GCLParser.fsp", "GCL.fsx", "GCLLexer.fs", "GCLParser.fs"

1. Place all key files in the same directory on your system

2. Note that the line of code in "GCL.fsx" (in our case: #r "C:/Users/pc/.nuget/packages/fslexyacc.runtime/7.0.6/lib/portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10/FsLexYacc.Runtime.dll")
specifies the path to the "FsLexYacc.Runtime.dll" file which is likely to vary between systems.
As such the path may need to be changed to reflect the path relevant to your current system.

3. To run the parser/syntax check program, run the entire "GCL.fsx" script and the user
will be prompted to enter GCL code, and the program will print the corresponding string in yhe graphviz format if the input has valid syntax
according to our specified rules and grammar. Otherwise the program will get an error.

4. To try another input, simply run line 110 again (containg only "compute") and enter another string to be tested.

#Commands
1. brew install nuget
2. nuget install FsLexYacc -Version 10.0.0
3. mono FsLexYacc.10.0.0/build/fslex/net46/fslex.exe GCLParser.fsl --unicode
4. mono FsLexYacc.10.0.0/build/fsyacc/net46/fsyacc.exe GCLParser.fsp --module GCLParser
5. fsharpi FILENAME.fsx