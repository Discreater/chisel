import os
from pathlib import Path

def title2(s: str) -> str:
    return s[0].upper() + s[1:]

def handle(content: str) -> str:
    params = ["that"]
    cat_params = "no" if len(params) == 0 else ''.join([params[0], *[title2(i) for i in params[1:]]])
    sf = 'macro SourceInfoTransform.' + cat_params + 'Arg'
    print(sf)
    # param = k
    dot_params = ', '.join(params)

    begin = 0
    while len(content) != 0:
        pos = content.find(sf, begin)
        if pos == -1:
            break
        pre = content[begin:pos+len(sf)]
        def_pos = pre.rfind('def')
        if def_pos == -1:
            break
        fun = pre[def_pos:]
        print('|' + fun)
        first_par = fun.find('(')
        first_sq = fun.find('[')
        first_co = fun.find(':')
        min_l = [i for i in [first_par, first_sq, first_co] if i != -1]
        name_last = min(min_l)
        fun_name = fun[4:name_last]
        new_fun = 'inline ' + fun[:-len(sf)] + '{implicit val sourceInfo: SourceInfo = summonInline[SourceInfo]; do_'+fun_name
        if len(params) != 0:
            new_fun += '('+dot_params+')}'
        else:
            new_fun += '}'
        new_pre = pre[:def_pos] + new_fun
        print('[' + fun_name)
        content = content[:begin] + new_pre + content[pos+len(sf):]
        begin = begin + len(new_pre)
    return content

def getFirstFunName(fun: str) -> str:
    first_par = fun.find('(')
    first_sq = fun.find('[')
    first_co = fun.find(':')
    min_l = [i for i in [first_par, first_sq, first_co] if i != -1]
    name_last = min(min_l)
    return fun[:name_last]

log_file = open('x.log', 'w')
def log_error(reason: str, content: str):
    print("error in", reason)
    print(content)
    print('------------------')
    log_file.write("error in " + reason + '\n')
    log_file.write(content + '\n')
    log_file.write('-----log_file splitter-------------\n')

def handle2(content: str, macro: str) -> str:
    sf = 'macro '+ macro + '.'

    last_end = 0
    while len(content) != 0:
        macro_start = content.find(sf, last_end)
        if macro_start == -1:
            break
        macro_end = macro_start + content[macro_start:].find('\n')
        # from last macro (not included) to current macro (included)
        before_macro = content[last_end:macro_end]
        macro_def_pos = before_macro.rfind('def')
        if macro_def_pos == -1:
            log_error("macro def", before_macro)
            last_end = macro_end
            continue
        fun = before_macro[macro_def_pos:]
        fun_name = getFirstFunName(fun[4:])


        next_fun_start = content[macro_end:].find('def')
        if next_fun_start == -1:
            log_error("next fun not found of " + fun_name, content[macro_end:])
            last_end = macro_end
            continue
        if "do_" + fun_name != getFirstFunName(content[macro_end+next_fun_start+4:]):
            log_error("next fun not match of " + fun_name, content[macro_end:])
            last_end = macro_end
            continue
        next_fun_name_end = len("do_" + fun_name) + next_fun_start + 4
        content = content[:last_end] + before_macro[:macro_def_pos+4+len(fun_name)] + content[macro_end+next_fun_name_end:]
        last_end = last_end + macro_def_pos + 4 + len(fun_name)
    return content

def main():
    for sc in Path('core').rglob('*.scala'):
        with open(sc, 'r') as f:
            content = f.read()
            content = handle2(content, "SourceInfoTransform")
            content = handle2(content, "SourceInfoWhiteboxTransform")
        with open(sc, 'w') as f:
            f.write(content)
            

if __name__ == '__main__':
    main()
