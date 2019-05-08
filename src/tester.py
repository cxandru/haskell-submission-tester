#!/usr/bin/python3
#coding=utf-8

import os, subprocess, re, codecs
from os.path import join, isfile, isdir, basename, exists, dirname
from shlex import quote
from shutil import copytree, rmtree
from glob import glob
from sys import argv,stderr,exit
from normalizer import normalize_exc_submissions
from checker import gradeExcForSubmissionRetMaybeErr

#################global var#############
debug = True
######################################


def reset(intermediate_dir):
    """ removes all .msg files from the intermediate_dir"""
    files = glob(join(intermediate_dir, '*.(msg|xml)')) #TODO:See if this works
    for f in files:
        if isfile(f):
            os.remove(f)

def exc_to_subexc_and_stack_name_dFunc(exc_to_subexc_and_stack_name_d_eval_file):
    """reads in a map of the form {<exc_base_name> : ([<sub_exc_name>], <stack_project_root>)}"""
    with open(exc_to_subexc_and_stack_name_d_eval_file, mode='r', encoding="utf8",errors='ignore') as f_in:
        exc_to_subexc_and_stack_name_d_eval = f_in.read()
    return eval(exc_to_subexc_and_stack_name_d_eval)

def exc_to_subexc_alFunc(exc_to_subexc_and_stack_name_d):
    "return an association list with all excs to subexs, tupled."
    exc_to_subexc_al = []
    for key,value in exc_to_subexc_and_stack_name_d.items():
        subexcs, _stack_dir = value
        for subexc in subexcs:
            exc_to_subexc_al.append((key, subexc))
    return exc_to_subexc_al

def exc_to_subexc_dFunc(exc_to_subexc_and_stack_name_d):
    "strip the stack proj root"
    return {k: v[0] for k,v in exc_to_subexc_and_stack_name_d.items()}

def genWalkmaps(submissions, directory, exc_to_subexc_al, exc_to_subexc_d):

    # if more than one candidate file appears for submission, the last one found will be mapped - students should be smart enough to not submit several files.
    # {submission:[(exercise,abs_path_to_exc)]}
    subm_to_ep_d = { s:[] for s in submissions}
    # {exercise:[(submission, abs_path_to_exc)]}
    exc_to_sp_d = { e:[] for e in exc_to_subexc_al }
    #Walk along the longer dimension creating the maps.
    
    for submission in submissions:
        for path,dirs,files in os.walk(join(directory, submission)):
            # skip 'hidden' dirs that students tend to include
            # In gnu find this would be done more efficiently with pruning in advance
            if (re.search(r'__MACOSX', path) or
                re.search(r'/[.].+',    path)) :
                continue
            for exercise_file in files:
                ename = exercise_file[:-3]
                if ename in exc_to_subexc_d:
                    for subexc in exc_to_subexc_d[ename]:
                        abs_path_to_exc = join(path,exercise_file)
                        exercise = (ename, subexc)
                        subm_to_ep_d[submission].append((exercise,abs_path_to_exc))
                        exc_to_sp_d[exercise].append((submission,abs_path_to_exc))
    return (subm_to_ep_d, exc_to_sp_d)


def submissionsFunc(directory):
    """
    list of all submissions (ids) in directory, skipping 
    the folders <foo> without bewertung_<foo>.txt file inside
    """
    return [ basename(f) for f in glob(join(directory, '*'))
              if (isdir(f) and isfile(join(f,'bewertung_{}.txt'.format(basename(f))))) ]


def setup(submissions_dir, reference_stack_projects_dir, exc_to_subexc_and_stack_name_d_eval_file):
    """
    submissions_dir.. should be empty, except for submissions! #TODO check this
    Copies the submissions_dir to dirs 'Intermediate_Files', 'Results', located in
    submissions_dir/.. ;  
    Copies the stack_projects_dir to 'Tests' in
    submissions_dir/.. ;
    Normalizes all .hs files of the type H\d-\d.hs in 'Intermediate_Files';
    evaluates the exc_to_subexc_and_stack_name_d_eval_file to a dictionary;
    returns an ExerciseGradingContext object with all the info to start grading!
    """
    try:
        root_dir = dirname(submissions_dir)
        intermediate_dir = join(root_dir,'Intermediate_Files')
        results_dir = join(root_dir, 'Results')
        stack_projects_dir = join(root_dir, 'Tests')

        copytree(submissions_dir, results_dir)
        copytree(submissions_dir, intermediate_dir)
        copytree(reference_stack_projects_dir, stack_projects_dir)

        normalize_exc_submissions(intermediate_dir)
        
        
        exc_to_subexc_and_stack_name_d = exc_to_subexc_and_stack_name_dFunc(exc_to_subexc_and_stack_name_d_eval_file)
        

        return ExerciseGradingContext(intermediate_dir, results_dir, stack_projects_dir, exc_to_subexc_and_stack_name_d)
    except Exception as e:
        print(e)
        resetEnvironmentStatic(submissions_dir)
       

def resetEnvironmentStatic(submissions_dir):
    os.system("find {root_dir} -mindepth 1 ! -path '*{submissions_basename}*' -delete".format(
        root_dir=quote(dirname(submissions_dir))
        , submissions_basename=quote(basename(submissions_dir)))
    )



class ExerciseGradingContext:
    def __init__(self, intermediate_normalized_dir, results_dir, stack_projects_dir, exc_to_subexc_and_stack_name_d):
        """
        •intermediate_normalized_dir: Submissions in this dir have the 
        name specified in the exc_to_…_d, with module header S. Special splices/removes of imports not yet supported.
        Here the .xml then .msg files are dumped in the submission roots for all exs/subexs.
        •results_dir: Same structure as intermediate_normalized_dir, only with original files.
        here the bewertung_%.txt files reside where the content of .msg files is dumped 
        after execution of all tests.
        •stack_projects_dir: Here all submissions are symlinked to the S.hs file in the src dir
        then the test with the specific subexc name is executed. 
        •exc_to_subexc_and_stack_name_d: This dictionary tells us which submission files
        should be tested by mapping their name to a list of subexc_names and the the 
        stack_project_root for this exercise, relative to stack_projects_dir.
        """
        self.intermediate_normalized_dir = intermediate_normalized_dir
        self.results_dir = results_dir
        self.stack_projects_dir = stack_projects_dir
        
        self.exc_to_subexc_and_stack_name_d = exc_to_subexc_and_stack_name_d
        
        self.subm_to_ep_d, self.exc_to_sp_d = genWalkmaps(
            submissionsFunc(intermediate_normalized_dir)
            , intermediate_normalized_dir
            , exc_to_subexc_alFunc(exc_to_subexc_and_stack_name_d)
            , exc_to_subexc_dFunc(exc_to_subexc_and_stack_name_d)
        )

    def gradeExc(self, exercise):
        """
        Grades this exc for all submission that have it.
        """
        exc_name,subexc_name = exercise
        _subexcs, stack_project_root = self.exc_to_subexc_and_stack_name_d[exc_name]
        for key in self.exc_to_sp_d[exercise]:
            submission, abs_path_to_exc = key
            maybeErr = gradeExcForSubmissionRetMaybeErr(exercise, submission, abs_path_to_exc, self.intermediate_normalized_dir, join(self.stack_projects_dir, stack_project_root))
            if(maybeErr and debug):
                print(maybeErr + "in submission " + abs_path_to_exc)
            else:
                print("submission "+ abs_path_to_exc + "correct!")

    def resetEnvironment(self):
        """can be called to leave behind a clean environment when exiting.
        """
        resetEnvironmentStatic(self.submissions_dir)





# for submission in submissions: #this line is optional
#     # write message(s) to "korrektur_xy"-file:
#     korr_file=join(result_dir, submission, 'bewertung_%s.txt'%submission)
#     msg = ""
#     for exercise in testable_exercises_basenames_hs:
#         full_name=exercise[:-3]
#         msg_file=join(intermediate_dir, submission, full_name+'.msg')
        
#         if isfile(msg_file):
#             with open(msg_file, 'r') as msg_fh:
#                 msg+=msg_fh.read()
#         else:
#             msg+="Tests for %s failed: file %s not found!\n"% (full_name, exercise)

#     if isfile(korr_file):
#         with open(korr_file, 'r') as korr_in:
#             korr=korr_in.read()
#         with open(korr_file, 'w') as korr_out:
#             adc=r'(Kommentare:)|(=========== Beginn der Kommentare ===========)'
#             edc='============ Ende der Kommentare ============\n'
#             startMatch=re.search(adc, korr)
#             if startMatch.end()<0:
#                 print("ERROR: %s malformed" % korr_file)
#             beg=startMatch.end()
#             korr_new=korr[:beg] + '\n' + msg + '\n' + edc
#             korr_out.write(korr_new)



# stats_comp_errors = { e:[] for e in testable_exercises_basenames_hs}
# stats_tested = { e:[] for e in testable_exercises_basenames_hs}

# if __name__ == '__main__':
#     if len(argv)==2:
#         if argv[1]=="debug":
#             debug=True
#         elif argv[1]=="reset":
#             files = glob(join(intermediate_dir, '*')) + glob(join(results_dir, '*'))
#             for f in files:
#                 if isfile(f):
#                     os.remove(f)
#                 if isdir(f):
#                     rmtree(f)
#             exit(0)
#         else:
#             print(usage,file=sys.stderr)
#             exit(1)

#     if len(argv)>2:
#         print(usage,file=sys.stderr)
#         exit(1)




# # clean up outside or intermediate_dir
# #~ for f in [join(path, exercise[:-3] + suffix) for exercise in testable_exercises_basenames_hs 
#                                              #~ for suffix in ['.hi','.o']
#                                              #~ for path in [test_dir, solution_dir]]:
#     #~ if exists(f):
# os.system('rm -rf %s' % intermediate_dir)

# for e in testable_exercises_basenames_hs:
#     print("\n##### " + e + ":")
#     print("  Does not compile (%d):"%len(stats_comp_errors[e]), stats_comp_errors[e])
#     print("  Message written (%d):"%len(stats_tested[e]), stats_tested[e])

# #untested = [s for s in submissions if s not in stats_tested[]
# #print "\nUntested (%d):"%len(untested), untested
