from hy.importer import import_file_to_ast, ast_compile, hy_eval
from hy.compiler import HyTypeError
from hy.lex import LexException
import hy.macros
import imp, sys, os.path

def repl_import(filename, code, globals_d, context_name, lang = "hy"):
    """'code' should be a Hy sexp or None. If it's None, the
    file 'filename' is opened to get the code to evaluate, and
    None is returned. If it's a Hy sexp, it's evaluated and the
    result is returned.

    'lang' can be "hy" or "python" when 'code' is None, but must
    be 'hy' when 'code' is not None.

    We use different semantics from import *. In particular,
    __all__ is ignored.
   
    The goal of this somewhat involved method (including the
    module daylight-hy.load) is to ensure that:
    - (def x 15) (defn foo [] x) followed by (foo) at the command
      line works
    - After loading two files, you can access variables defined in
      file 1 that are shadowed by variables defined in file 2."""
    mname = 'daylight-' + os.path.dirname(os.path.abspath(filename))
    m = None
    try:
        if code is None:
            preexisting = mname in sys.modules
            try:
                if lang == "hy":
                    _ast = import_file_to_ast(filename, mname)
                m = (sys.modules[mname]
                    if preexisting
                    else imp.new_module(mname))
                m.__file__ = filename
                if lang == "hy":
                    eval(ast_compile(_ast, filename, "exec"), m.__dict__)
                elif lang == "python":
                    exec(compile(open(filename).read(), filename, 'exec'), m.__dict__)
                else:
                    raise ValueError("Unknown language: {}".format(lang))
                sys.modules[mname] = m
            except (HyTypeError, LexException) as e:
                if e.source is None:
                    with open(filename, 'rt') as fp:
                        e.source = fp.read()
                    e.filename = filename
                raise
            except Exception:
                if not preexisting:
                    sys.modules.pop(mname, None)
                raise
            returnval = None
        else:
            if lang != 'hy':
                raise NotImplementedError
            m = sys.modules.get(mname)
            if not m:
                m = imp.new_module(mname)
                sys.modules[mname] = m
            returnval = hy_eval(code, m.__dict__, mname)
    finally:
        if m is not None:
            # Copy the module's names (except for names starting with "_")
            # into our globals.
            for k in dir(m):
               if not k.startswith('_'):
                   globals_d[k] = getattr(m, k)
    # Import macros.
    hy.macros.require(mname, context_name, "ALL")
    return returnval
