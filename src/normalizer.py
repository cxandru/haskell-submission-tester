import os, re, codecs
from os.path import join, isfile, isdir, basename, exists

def normalize_exc_submissions(directory):
    '''Normalizes student submission file names to the schema H\d{1,2}_\d.hs and
    adds a module header with the appropriate name to allow qualified imports of the
    submission file.
    '''
    for path,dirs,files in os.walk(directory):
        for filename in files:
            ##normalize file name
            new_filename=filename
            new_filename = new_filename.strip()
            if new_filename[-6:]=='.hs.hs':
                new_filename=new_filename[:-3]
            if new_filename[-7:]=='.hs.txt':
                new_filename=new_filename[:-4]

            if re.match(r'[AhH]?\d{1,2}[-_]\d[.][hH][sS]', new_filename):
                new_filename= 'H' + new_filename[1:-3].replace('_','-') + '.hs'

            path_to_normal_exc = join(path,new_filename)
            if filename != new_filename:
                if not exists(path_to_normal_exc):
                    os.rename(join(path,filename), path_to_normal_exc)
                else:
                    #The Overwrote thingy is no longer applicable
                    print("WARNING: Overwrote file %s" % path_to_normal_exc) # very unlikely

            with open(path_to_normal_exc, mode='r',encoding="utf8",errors='ignore') as f_in:
                # stip Unicode BOMs as GHC might not like them
                s = f_in.read()
                if s[:3] == codecs.BOM_UTF8:
                    s = s[3:]
                #remove module declarations. 
                s=re.sub(r'module\s+(.+?)\s+where', r'', s, count=1, flags=re.DOTALL)
                #Put the module header after any file-level pragmas -
                #This means we allow all language extensions
                #There should not reasonably be any uses of the INCLUDE pragma.
            
                pragmas = list(re.findall(r'{-#\s+(?:LANGUAGE|OPTIONS_GHC).*?-#}',r'', flags=re.DOTALL))
                if pragmas:
                    last_pragma_end = pragmas[-1].end()
                else:
                    last_pragma_end = 0
                #insert module header after the last file pragma.
                s=s[:last_pragma_end] + 'module S where\n\n' + s[last_pragma_end:]
                with open(path_to_normal_exc, mode='w',encoding="utf8",errors='ignore') as f_out:
                    f_out.write(s)
                    # note that haskell requires TWO newlines after module line
