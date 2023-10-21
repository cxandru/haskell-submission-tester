#!/usr/bin/python3
#coding=utf-8

import os, subprocess, re, codecs
from os.path import join, isfile, isdir, basename, exists, dirname, islink
from shlex import quote
from shutil import copytree, rmtree
from glob import glob
from sys import argv,stderr,exit
from normalizer import normalize_exc_submissions
from checker import gradeExcForSubmissionRetMaybeErr
from itertools import chain
import logging

#################Logging##############
logging.basicConfig(level=logging.NOTSET)
######################################

def resetStaticTest(reference_stack_projects_dir, test_execution_dir, stack_project_root):
    """ re-import the specified testing project from the reference dir to the actual """
    actual_project = join(test_execution_dir,stack_project_root)
    if isdir(actual_project) :
        rmtree(actual_project)
        logging.info("Removing " + actual_project)
    else: logging.info(actual_project + "did not exist.")
    logging.info("Resetting " + actual_project + " from reference dir '" + reference_stack_projects_dir + "'")
    copytree(join(reference_stack_projects_dir,stack_project_root), actual_project)


def resetExcStatic(intermediate_dir, submissions, exercise):
    """ per-exercise reset"""
    exc_module_name,subexc_name = exercise
    exc_module_name = exc_module_name + subexc_name
    files = [glob(join(intermediate_dir,submission, exc_module_name+'.msg')) + glob(join(intermediate_dir,submission, exc_module_name+'.xml')) for submission in submissions]
    for f in chain.from_iterable(files):
        if isfile(f):
            logging.info(f)
            os.remove(f)

def resetAllExcsStatic(intermediate_dir, submissions):
    """ removes all .msg and .xml files from the intermediate_dir"""
    files = [glob(join(intermediate_dir,submission, '*.msg')) + glob(join(intermediate_dir,submission, '*.xml')) for submission in submissions]
    for f in chain.from_iterable(files):
        if isfile(f):
            logging.info(f)
            os.remove(f)

def exc_to_subexc_and_stack_names_dFunc(exc_to_subexc_and_stack_names_d_eval_file):
    """reads in a map of the form {<exc_base_name> : ([<sub_exc_name>], <stack_project_root>)}"""
    with open(exc_to_subexc_and_stack_names_d_eval_file, mode='r', encoding="utf8",errors='ignore') as f_in:
        exc_to_subexc_and_stack_names_d_eval = f_in.read()
    return eval(exc_to_subexc_and_stack_names_d_eval)

def exc_to_subexc_alFunc(exc_to_subexc_and_stack_names_d):
    "return an association list with all excs to subexs, tupled."
    exc_to_subexc_al = []
    for key,values in exc_to_subexc_and_stack_names_d.items():
        for value in values:
          subexcs, _stack_dir = value
          for subexc in subexcs:
              exc_to_subexc_al.append((key, subexc))
    return exc_to_subexc_al

def exc_to_subexc_dFunc(exc_to_subexc_and_stack_names_d):
    "strip the stack proj root"
    exc_to_subexc_d = {}
    for k,es_sts in exc_to_subexc_and_stack_names_d.items():
        e = exc_to_subexc_d.setdefault(k,[])
        for es_st in es_sts:
            e.extend(es_st[0])
    return exc_to_subexc_d

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
                if exercise_file[-3:] in [ "zip" "rar" ".gz" ".7z" ]:
                    logging.warning(submission + "contains an archive. Did you unzip?" )
                ename = exercise_file[:-3]
                if ename in exc_to_subexc_d:
                    for subexc in exc_to_subexc_d[ename]:
                        abs_path_to_exc = join(path,exercise_file)
                        exercise = (ename, subexc)
                        subm_to_ep_d[submission].append((exercise,abs_path_to_exc))
                        exc_to_sp_d[exercise].append((submission,abs_path_to_exc))
    return (subm_to_ep_d, exc_to_sp_d)

def submissionsFuncBewertung(directory):
    """
    list of all submissions (ids) in directory, skipping 
    the folders <foo> without bewertung_<foo>.txt file inside
    """
    return [ basename(f) for f in glob(join(directory, '*'))
              if (isdir(f) and isfile(join(f,'bewertung_{}.txt'.format(basename(f))))) ]

def submissionsFuncFeedback(directory):
    """
    list of all submissions (ids) in directory, skipping
    the folders without feedback.txt file inside
    """
    return [ basename(f) for f in glob(join(directory, '*'))
              if (isdir(f) and isfile(join(f,'feedback.txt'))) ]


def setup_dir_default(base_dir):
    return setup_dir(base_dir, "Submissions", "Tests", "exc_dict")

def setup_dir(base_dir, submissions, reference_stack_projects_dir, exc_to_subexc_and_stack_names_d_eval_file):
    return setup(join(base_dir,submissions), join(base_dir,reference_stack_projects_dir), join(base_dir, exc_to_subexc_and_stack_names_d_eval_file))

def setup(submissions_dir, reference_stack_projects_dir, exc_to_subexc_and_stack_names_d_eval_file):
    """
    If the directories don't already exist
    Copies the `submissions_dir` to dirs 'Intermediate_Files', 'Results', located in
    submissions_dir/.. ;  
    Copies the `reference_stack_projects_dir` to 'Test_Execution' in
    submissions_dir/.. ;
    Normalizes all to-be-tested .hs files in 'Intermediate_Files';
    evaluates the exc_to_subexc_and_stack_names_d_eval_file to a dictionary;
    returns an ExerciseGradingContext object with all the info to start grading!
    """
    try:
        root_dir = dirname(submissions_dir)
        intermediate_dir = join(root_dir,'Intermediate_Files')
        results_dir = join(root_dir, 'Results')
        test_execution_dir = join(root_dir, 'Test_Execution')

        if not isdir(results_dir): copytree(submissions_dir, results_dir)
        exc_to_subexc_and_stack_names_d = exc_to_subexc_and_stack_names_dFunc(exc_to_subexc_and_stack_names_d_eval_file)
        if not isdir(intermediate_dir):
            copytree(submissions_dir, intermediate_dir)
            normalize_exc_submissions(intermediate_dir, exc_to_subexc_and_stack_names_d)
        #TODO: use find to recursively symlink all dirs except for src, which we copy.
           #This means we react to changes in the original test dir, but don't overwrite src, which is important.
           #Currently the way to react to changes is to use `resetTest`.
        #Only copy the tests stated as used in the exc_d map:
        for excList_projects in exc_to_subexc_and_stack_names_d.values():
            for excList_project in excList_projects:
              _el, project = excList_project
              #Don't copy tests to test execution if tests dir exists
              if not isdir(join(test_execution_dir, project)):
                  copytree(join(reference_stack_projects_dir, project), join(test_execution_dir, project))

        return ExerciseGradingContext(intermediate_dir, results_dir, reference_stack_projects_dir, test_execution_dir, exc_to_subexc_and_stack_names_d)
    except Exception as e:
        logging.error(e)
        rmtree(intermediate_dir)


class ExerciseGradingContext:
    def __init__(self, intermediate_normalized_dir, results_dir, reference_stack_projects_dir, test_execution_dir, exc_to_subexc_and_stack_names_d):
        """
        •intermediate_normalized_dir: Submissions in this dir have the 
        name specified in the exc_to_…_d, with module header S. Special splices/removes of imports not yet supported.
        Here the .xml then .msg files are dumped in the submission roots for all exs/subexs.
        •results_dir: Same structure as intermediate_normalized_dir, only with original files.
        here the bewertung_%.txt files reside where the content of .msg files is dumped 
        after execution of all tests.
        •test_execution_dir: Here all submissions are symlinked to the S.hs file in the src dir
        then the test with the specific subexc name is executed. 
        •exc_to_subexc_and_stack_name_d: This dictionary tells us which submission files
        should be tested by mapping their name to a list of subexc_names and the the 
        stack_project_root for this exercise, relative to test_execution_dir.
        """
        self.intermediate_normalized_dir = intermediate_normalized_dir
        self.results_dir = results_dir
        self.reference_stack_projects_dir = reference_stack_projects_dir
        self.test_execution_dir = test_execution_dir
        
        self.exc_to_subexc_and_stack_names_d = exc_to_subexc_and_stack_names_d

        submissions = submissionsFuncFeedback(intermediate_normalized_dir)
        if not submissions:
            logging.warning("No submissions!")
        
        # {submission:[(exercise,abs_path_to_exc)]}
        # {exercise:[(submission, abs_path_to_exc)]}
        self.subm_to_ep_d, self.exc_to_sp_d = genWalkmaps(
            submissions
            , intermediate_normalized_dir
            , exc_to_subexc_alFunc(exc_to_subexc_and_stack_names_d)
            , exc_to_subexc_dFunc(exc_to_subexc_and_stack_names_d)
        )

    def gradeExc(self, exercise):
        """
        Grades this exc for all submission that have it.
        """
        exc_module_name,subexc_name = exercise
        subexcs2roots = self.exc_to_subexc_and_stack_names_d[exc_module_name]
        stack_project_root = [root for subexcs,root in subexcs2roots if subexc_name in subexcs][0]
        for key in self.exc_to_sp_d[exercise]:
            submission, abs_path_to_exc = key
            #skip files for which we already have generated a msg file.
            #if this is not wanted, reset should be called (per exercise).
            if isfile(join(self.intermediate_normalized_dir,submission,exc_module_name+subexc_name+'.msg')):
                logging.info("Skipping "+abs_path_to_exc)
                continue
            maybeErr = gradeExcForSubmissionRetMaybeErr(exercise, submission, abs_path_to_exc, self.intermediate_normalized_dir, join(self.test_execution_dir, stack_project_root))
            if(maybeErr):
                logging.info(maybeErr + "in submission " + abs_path_to_exc)
            else:
                logging.info("Submission "+ abs_path_to_exc + " correct!")

    def resetAllExcs(self):
        resetAllExcsStatic(self.intermediate_normalized_dir,self.subm_to_ep_d.keys())

    def resetExc(self, exercise):
        resetExcStatic(self.intermediate_normalized_dir,self.subm_to_ep_d.keys(),exercise)

    def resetTest(self,stack_project_root):
        resetStaticTest(self.reference_stack_projects_dir,self.test_execution_dir, stack_project_root)

    def gradeAllExcAndWriteOut(self):
        for exc in self.exc_to_sp_d.keys():
            self.gradeExc(exc)
            logging.info("Finished grading {}".format(exc))
        self.genBewewertungenFromMsgs()
        logging.info("Finished generating Bewertungen")

        
    def genBewewertungenFromMsgs(self):
        for submission in self.subm_to_ep_d.keys():
            self.concatMsgsToFinalAndRepaceResFeedbackFile(submission)

    def concatMsgsToFinalAndRepaceResFeedbackFile(self,submission):
        allMsgs=""
        for exercise in self.exc_to_sp_d.keys():
            exc_module_name, subexc_name = exercise
            msg_f = join(self.intermediate_normalized_dir, submission, exc_module_name+subexc_name+'.msg')
            if isfile(msg_f): #TODO: do we write if a student didn't hand in a file?
                with open(msg_f, mode='r') as f_in:
                    allMsgs += "\n"+ f_in.read()
            else:
                logging.warning("msg file " + msg_f + " does not exist")

        emptyBewFile=join(self.intermediate_normalized_dir, submission ,'feedback.txt')
        targetBewFile=join(self.results_dir,submission, 'feedback.txt')
        if isfile(emptyBewFile):
            with open(emptyBewFile, 'r') as b_e:
                emptyBewContents=b_e.read()
            adc=r'Feedback:'
            startMatch=re.search(adc, emptyBewContents)
            if not startMatch or startMatch.end()<0:
                logging.error("{} malformed".format(emptyBewFile))
            beg=startMatch.end()
            targetBewContents=emptyBewContents[:beg] + '\n' + allMsgs + '\n' + emptyBewContents[beg:]

            if isfile(targetBewFile): os.remove(targetBewFile)
            with open(targetBewFile, 'w') as b_t:
                b_t.write(targetBewContents)


    def concatMsgsToFinalAndRepaceResBewertungFile(self,submission):
        allMsgs=""
        for exercise in self.exc_to_sp_d.keys(): #+glob(join(self.intermediate_normalized_dir,submission,'*.xml')
            exc_module_name, subexc_name = exercise
            msg_f = join(self.intermediate_normalized_dir, submission, exc_module_name+subexc_name+'.msg')
            if isfile(msg_f): #TODO: do we write if a student didn't hand in a file?
                with open(msg_f, mode='r') as f_in:
                    allMsgs += "\n"+ f_in.read()
                
        emptyBewFile=join(self.intermediate_normalized_dir, submission ,'bewertung_{}.txt'.format(submission))
        targetBewFile=join(self.results_dir,submission, 'bewertung_{}.txt'.format(submission))
        if isfile(emptyBewFile):
            with open(emptyBewFile, 'r') as b_e:
                emptyBewContents=b_e.read()
            adc=r'(Kommentare:)|(=========== Beginn der Kommentare ===========)'
            #edc='============ Ende der Kommentare ============\n'#so this is usually there but we throw it away and replace it? is that the reason?
            startMatch=re.search(adc, emptyBewContents)
            if not startMatch or startMatch.end()<0:
                logging.error("{} malformed".format(emptyBewFile))
            beg=startMatch.end()
            targetBewContents=emptyBewContents[:beg] + '\n' + allMsgs + '\n' #+ edc
            
            if isfile(targetBewFile): os.remove(targetBewFile)
            with open(targetBewFile, 'w') as b_t:
                b_t.write(targetBewContents)
