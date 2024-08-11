# This is a module created as an example for py_basics.ipynb

def add_nums(x, y):
    try:
        res = x + y
    except:
        print("Operation not possible")
        res = None
    return res

def div_nums(x, y):
    try:
        res = x/y
    except:
        print("Operation not possible")
        res = None
    return res