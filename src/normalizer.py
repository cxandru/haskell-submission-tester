import os, subprocess, re, codecs
from os.path import join, isfile, isdir, basename, exists
from shlex import quote

def findLastFileHeaderPragmaLine(code):
    pragmas = list(re.finditer(r'^\s*?{-#\s+?(?:LANGUAGE|OPTIONS_GHC).*?#-}',code, flags=re.DOTALL | re.MULTILINE))
    if pragmas:
        return pragmas[-1].end()
    else:
        return 0

def normalize_file_contents(code):
    if code[:3] == codecs.BOM_UTF8:
        code = code[3:]
    #remove module declarations. 
    code=re.sub(r'module\s+(.+?)\s+where', r'', code, flags=re.DOTALL)
    #Put the module header after any file-level pragmas -
    #This means we allow all language extensions
    #There should not reasonably be any uses of the INCLUDE pragma.

    last_pragma_end = findLastFileHeaderPragmaLine(code)
    #insert module header after the last file pragma.
    return code[:last_pragma_end] + '\nmodule S where\n\n' + code[last_pragma_end:]
    
    
def normalize_exc_submissions(directory):
    '''Normalizes student submission file names to the schema H\d{1,2}_\d.hs and
    adds a module header with the appropriate name to allow qualified imports of the
    submission file.
    '''
    subprocess.run(["chmod", "-R", "u+w", quote(directory)])
    walker=os.walk(directory)
    next(walker)#start at depth 2
    for path,dirs,files in walker:
        if (re.search(r'__MACOSX', path) or
            re.search(r'/[.].+',    path)) :
            continue
        
        for filename in files:
            #normalize file name
            new_filename=filename
            new_filename = new_filename.strip()
            if new_filename[-6:]=='.hs.hs':
                new_filename=new_filename[:-3]
            if new_filename[-7:]=='.hs.txt':
                new_filename=new_filename[:-4]
                
            #looks like an intended submission
            if re.match(r'[AhH]?\d{1,2}[-_]\d[.][hH][sS]', new_filename):
                new_filename= 'H' + new_filename[1:-3].replace('_','-') + '.hs'
            else: continue

            path_to_normal_exc = join(path,new_filename)
            if filename != new_filename:
                if not exists(path_to_normal_exc):
                    os.rename(join(path,filename), path_to_normal_exc)

            with open(path_to_normal_exc, mode='r',encoding="utf8",errors='ignore') as f_in:
                # stip Unicode BOMs as GHC might not like them
                s = normalize_file_contents(f_in.read())
                with open(path_to_normal_exc, mode='w',encoding="utf8",errors='ignore') as f_out:
                    f_out.write(s)
