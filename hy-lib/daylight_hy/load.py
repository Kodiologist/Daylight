from hy.importer import import_file_to_ast, ast_compile, hy_eval
from hy.compiler import HyTypeError
from hy.lex import LexException
import hy.macros
import imp, sys, os.path

def import_file_to_module(module_name, fpath, lang):
    """Import content from fpath and put it into a Python module.
    If module_name already exists, the preexisting module
    is updated instead of a new module being created.
    If a new module is created, it is installed in sys.modules.

    Returns the module."""
    preexisting = module_name in sys.modules
    try:
        if lang == "hy":
            _ast = import_file_to_ast(fpath, module_name)
        mod = (sys.modules[module_name]
            if preexisting
            else imp.new_module(module_name))
        mod.__file__ = fpath
        if lang == "hy":
            eval(ast_compile(_ast, fpath, "exec"), mod.__dict__)
        elif lang == "python":
            execfile(fpath, mod.__dict__)
        else:
            raise ValueError("Unknown language: {}".format(lang))
        sys.modules[module_name] = mod
    except (HyTypeError, LexException) as e:
        if e.source is None:
            with open(fpath, 'rt') as fp:
                e.source = fp.read()
            e.filename = fpath
        raise
    except Exception:
        if not preexisting:
            sys.modules.pop(module_name, None)
        raise
    return mod

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
    if code is None:
        m = import_file_to_module(mname, filename, lang)
        returnval = None
    else:
        if lang != 'hy':
            raise NotImplementedError
        m = sys.modules.get(mname)
        if not m:
            m = imp.new_module(mname)
            sys.modules[mname] = m
        returnval = hy_eval(code, m.__dict__, mname)
    # Copy the module's names (except for names starting with "_")
    # into our globals.
    for k in dir(m):
       if not k.startswith('_'):
           globals_d[k] = getattr(m, k)
    # Import macros, too.
    hy.macros.require(mname, context_name)
    return returnval
