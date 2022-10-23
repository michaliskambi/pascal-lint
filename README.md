Pascal lint (code analysis for potential errors)

**TODO: The current project is just a test of `fcl-passrc`, nothing more. The checks documented below are NOT yet implemented. After planning this project, I simply didn't find the time to make it actually happen. Help (PRs) is most welcome!**

# What it does

This tool parses your modern Object Pascal code and tries to find common mistakes (that are undetected by compiler for various reasons).

The checks done right now are:

1. Check the validity of parameters for Format (http://www.freepascal.org/docs-html/rtl/sysutils/format.html) and friends. The goal is to warn you about invocations of Format that will *definitely* cause an EConvertError warning at runtime. For example this code:

    ```
    Format('%s', [123])
    ```

    compiles fine, but will cause a EConvertError exception at runtime (since placeholder "%s" doesn't match an Integer "123"; you should have used "%s" instead). It checks the subset of Format calls -- when the format pattern is constant, and the number and types of arguments are (at least partially known).

    It detects not only Format, but also CreateFmt methods of the Exception class, and (if you use --castle-engine-extensions option) the WritelnLog and WritelnWarning methods.

    LIMITATION: Note that we only have a parser, we don't resolve the symbols between units like a compiler does. So the checker detects the procedures by looking for names "Format", "CreateFmt". If you define in your own units a procedure called "Format" or a method called "CreateFmt", it will check it too, since it doesn't know whether the identifier Format that it sees comes from SysUtils or your custom unit.

    TODO: Use PasResolver to resolve symbols?

2. Check that the constructor and destructors contain an `inherited` call. Forgetting the `inherited` call is an easy mistake, and in 99% cases it should be there. Either `inherited;` or `inherited Create ... / Destroy ...;` will be fine.

    LIMITATION: We only have a parser, we cannot do code flow analysis. It would be beneficial to extend this check to _"all code paths must call the inherited constructor / destructor"_, not just _"a call to the inherited constructor / destructor is somewhere inside"_. But we simply cannot do this without implementing a code flow analysis on top of the parser information, and implementing that is not something within the scope of this tool.

    TODO: A way to mark some constructor / destructor explicitly as "I know what I'm doing, don't signal a warning here because it doesn't need inherited".

    TODO: A way to mark some other methods as "overridden must call inherited" would be useful. This way we could extend this check to the specific routines. Many virtual methods don't need to have an "inherited" call, but some of them do (the class will not function properly if you forget an "inherited" call). So it would be nice to be able to extend this check for them.

3. Check `FreeAndNil` is only done on something that is definitely an object instance.

    The goal is to capture such invalid code that unfortunately compiles:

    ```pascal
    uses SysUtils;
    var
      I: Integer;
      W: Word;
    begin
      FreeAndNil(I);
      FreeAndNil(W); // W doesn't even have the same size as Pointer, but stupid FreeAndNil still compiles!
    end.
    ```

    LIMITATION: The check will capture only some invalid cases. Again, we're just a parser, we do not resolve the types of variables with 100% reliability.

    TODO: Use PasResolver to resolve symbols?

4. Warn when for-loop counter variable being used outside of the loop, and the loop has no "Break". The loop variable value is undefined if you exit the loop without `Break`.

    The goal is to capture such invalid code that unfortunately compiles:

    ```pascal
    var
      I: Integer;
    begin
      for I := 0 to 10 do
        Writeln(I);
      Writeln('Value of I is undefined now: ', I);
    end.
    ```

5. Checks that if you overload the constructor, you have also overloaded the parameterless constructor (or you have secured from it already by making non-public constructor).

    See
    https://stackoverflow.com/questions/14003153/how-to-hide-the-inherited-tobject-constructor-while-the-class-has-overloaded-one
    http://andy.jgknet.de/blog/2011/07/hiding-the-tobject-create-constructor/

6. Check you don't call `Exception.Create` instead of `raise Exception.Create`. It is an easy mistake to forget the `raise` keyword.

7. Check for ";;", two semicolons one after the other. This is usually a typo.

8. Check you don't call constructor as a regular method. This causes wild bugs, be reinitializing already-initialized instance, and the constructors (and general class logic) are in general *not* ready for this.

    The goal is to capture such invalid code that unfortunately compiles:

    ```pascal
    var
      O: TObject;
    begin
      O := TObject.Create;
      O.Create; // calling constructor like this is invalid
      O.Free;
    end.
    ```

# Downloading

Since you're a Pascal programmer,

1. just grab the source code from GitHub (you can download using Git, SVN or just download a zip),

2. and compile by executing "make" inside.

You need to have FPC installed and the "fpc" command must be available on $PATH. Tested with FPC >= 3.2.0 now.

# Using

In the simplest form, just run

```
pascal-lint myunit1.pas myunit2.pas
```

You can of course use a shell script, or a batch script, to construct and pass a list of your units in any way you like. If you're on Unix, this may help:

```
pascal-lint my-code/*.pas
pascal-lint `find -iname *.pas`
```

The tool supports a number of command-line options. See `--help`. The unhandled options are passed to the fcl-passrc engine, and in effect the tool supports also a number of command-line options you know from FPC that influence parsing. See below.

# Parsing

The code is parsed using a cool parser included in the FPC standard units: fcl-passrc. In case the parsing failed, we show you the parser error message. If your code compiles (with FPC or Delphi), but fails to be parsed by fcl-passrc -- please submit a bugreport to http://bugs.freepascal.org. This way you're not only helping the pascal-lint, you're helping the fcl-passrc to be as complete as possible!

The fcl-passrc engine, and so this tool, supports a subset of FPC syntax options, to influence the parsing. In particular:

- Use -Fixxx to indicate directories with include files (to resolve {$I xxx.inc} in your code).
- Use -Mxxx to indicate fpc mode, like -Mobjfpc or -Mdelphi.
- Use various syntax options, e.g. -Sc to support C-like operators "+=" and friends.

# License

GNU GPL >= 2.
Copyright Michalis Kamburelis.
Pull requests most welcome.
