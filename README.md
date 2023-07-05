# agda-gpt-assistant

Before start, you have to prepare directory with templates. Directory is
available in git repositry * /data/templates *  Copy this dir in your currnet
executing dix1r, or * ~/.agda-gpt-experiment * witch one you need to create
first.

Example: aga -i=dir -s= Problems/ExampleProblems/ -c=myConfig.json -m=Pretty
-l=15

aga [OPTIONS]

Common flags:
  -i --input=ITEM   This flag defines the problems located in the
                    agda-problem-repository for AGA to solve. It can have one
                    of three values. It can point to a single problem - *
                    problem *, the entire directory of problems including
                    subdirectories and their contents - * dir *, or a list of
                    selected problems from the repository placed in a .json
                    file - * list *. This flag does not have a default value.
  -s --source=ITEM  The problem name always consists of the full path to the
                    Agda file, starting from the Problems directory in the Agda
                    problem base repository, for example,
                    Problems/ExampleProblems/P1.agda.
                    If you specified a specific problem to solve, you must
                    provide its path starting from the main Problems directory,
                    following the general principle of pointing to a problem.
                    If you specified a list of problems, it should be located
                    in your current directory and saved in a file named
                    problemslist.json. The location of each problem on the list
                    to be solved must adhere to the general guidelines for
                    pointing to a problem, such as
                    Problems/ExampleProblems/P1.agda.
                    If you chose dir as the input, you must specify an existing
                    directory from the problem base. It can also be just the
                    Problems directory, in which case all the problems in the
                    problem repository will be indicated for solving.
  -c --conf=FILE    this is a config file, it should be in the current
                    directory or * ~/.agda-gpt-experiment * default value for
                    this flag is config.json
  -m --mode=MODE    Choose one of the operating modes. * Pretty * or
                    * Debug *  - which has more details. The default value for
                    this flag is Pretty.
  -l --maxt=NUMBER  Set this flag to specify the number of round
                    conversations with ChatGPT for each problem. This flag has
                    a default value of 5.
  -? --help         Display help message
  -V --version      Print version information
