import hy
from hy.reader.exceptions import LexException
from hy.compiler import ast_compile, hy_compile, hy_eval
from hy.errors import HyTypeError
import hy.macros
import types, sys, os.path

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
                with open(filename) as o:
                    text = o.read()
                m = (sys.modules[mname]
                    if preexisting
                    else types.ModuleType(mname))
                m.__file__ = filename
                sys.modules[mname] = m
                if lang == "hy":
                    _ast = hy_compile(hy.read_many(text), m)
                    eval(ast_compile(_ast, filename, "exec"), m.__dict__)
                elif lang == "python":
                    exec(compile(text, filename, 'exec'), m.__dict__)
                else:
                    raise ValueError("Unknown language: {}".format(lang))
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
                m = types.ModuleType(mname)
                sys.modules[mname] = m
            returnval = hy.eval(code, m.__dict__, module = m)
    finally:
        if m is not None:
            # Copy the module's names (except for names starting with "_")
            # into our globals.
            for k in dir(m):
               if not k.startswith('_'):
                   globals_d[k] = getattr(m, k)
    # Import macros.
    hy.macros.require(m, sys.modules[context_name], "ALL")
    return returnval
