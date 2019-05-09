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

#################global var#############
debug = True
######################################

def resetStatic(intermediate_dir, submissions):
    """ removes all .msg files from the intermediate_dir"""
    files = [glob(join(intermediate_dir,submission, '*.msg')) + glob(join(intermediate_dir,submission, '*.xml')) for submission in submissions]
    for f in chain.from_iterable(files):
        if isfile(f):
            if debug: print(f)
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
    If the directories already exist
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
        stack_projects_dir = join(root_dir, 'Test_Execution')

        if not isdir(results_dir): copytree(submissions_dir, results_dir)
        if not isdir(intermediate_dir):
            copytree(submissions_dir, intermediate_dir)
            normalize_exc_submissions(intermediate_dir)
        #todo: use find to recursively symlink all dirs except for src, which we copy.
        #This means we react to changes in the original test dir, but don't overwrite src, which is important.
        if not isdir(stack_projects_dir): copytree(reference_stack_projects_dir, stack_projects_dir)
        
        
        exc_to_subexc_and_stack_name_d = exc_to_subexc_and_stack_name_dFunc(exc_to_subexc_and_stack_name_d_eval_file)

        return ExerciseGradingContext(intermediate_dir, results_dir, stack_projects_dir, exc_to_subexc_and_stack_name_d)
    except Exception as e:
        print(e)
        os.rmtree(intermediate_dir)


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
                print("submission "+ abs_path_to_exc + " correct!")

    def reset(self):
        resetStatic(self.intermediate_normalized_dir,self.subm_to_ep_d.keys())
    
    def genBewewertungenFromMsgs(self):
        for submission in self.subm_to_ep_d.keys():
            self.concatMsgsToFinalAndRepaceResBewertungFile(submission)
                
    def concatMsgsToFinalAndRepaceResBewertungFile(self,submission):
        allMsgs=""
        #do we write if a student didn't hand in a file? 
        for msg_f in glob(join(self.intermediate_normalized_dir,submission,'*.msg'))+glob(join(self.intermediate_normalized_dir,submission,'*.xml')):
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
                print("ERROR: {} malformed".format(emptyBewFile))
            beg=startMatch.end()
            targetBewContents=emptyBewContents[:beg] + '\n' + allMsgs + '\n' #+ edc
            
            if isfile(targetBewFile): os.remove(targetBewFile)
            with open(targetBewFile, 'w') as b_t:
                b_t.write(targetBewContents)
