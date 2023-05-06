import os
from pathlib import Path

def title2(s: str) -> str:
    return s[0].upper() + s[1:]

def main():
    params = ["that"]
    cat_params = "no" if len(params) == 0 else ''.join([params[0], *[title2(i) for i in params[1:]]])
    sf = 'macro SourceInfoWhiteboxTransform.' + cat_params + 'Arg'
    print(sf)
    # param = k
    dot_params = ', '.join(params)
    for sc in Path('.').rglob('*.scala'):
        with open(sc, 'r') as f:
            content = f.read()
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
                new_fun = 'inline ' + fun[:-len(sf)] + '{given sourceInfo: SourceInfo = summonInline[SourceInfo]; do_'+fun_name
                if len(params) != 0:
                    new_fun += '('+dot_params+')}'
                else:
                    new_fun += '}'
                new_pre = pre[:def_pos] + new_fun
                print('[' + fun_name)
                content = content[:begin] + new_pre + content[pos+len(sf):]
                begin = begin + len(new_pre)
        with open(sc, 'w') as f:
            f.write(content)
            

if __name__ == '__main__':
    main()
