from os.path import basename, dirname
from checker import gradeExcForSubmissionRetMaybeErr
from sys import argv

def testSub(path_to_normal_exc, path_to_stack_testing_proj, sub_exc_name):
    directory = dirname(path_to_normal_exc)
    filename = basename(path_to_normal_exc)
    exercise = (filename[:-3], sub_exc_name)
    maybeErr = gradeExcForSubmissionRetMaybeErr(exercise, "", path_to_normal_exc, directory, path_to_stack_testing_proj)
    return maybeErr or "korrekt!"

usage="""
arg1: path_to_normal_exc (module S where)
arg2: path_to_stack_testing_proj (src/S.hs will be overwritten for testing there)
arg3: sub_exc_name: the subexercise (name of the corresponding stack test target)
Files exc_name.xml and exc_name.msg are created as a side effect.
"""
if __name__ == "__main__":
    if len(argv)==1:
        print(usage)
    else:
        print(testSub(argv[1], argv[2], argv[3]))
    
    
