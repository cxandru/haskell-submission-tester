Let's list all the problems we had last iteration:

1) The hide-all-packages command and then linking the packages is
horrible bc it creates gigantic binary executables.  We would prefer
to have a stack project (possibly dependent on submission) where we
specify the packages in the environment.  Of course it would be much
nicer if this was specified in the exercises.
Essentially we would keep changing out the file main (maybe use
symlinks). Actually it would be great if the exercises were written as
literate haskell files, enabling extraction of problem statement. They
should also be modular. The proForma idea is horrible in every
instance though, excepted if you generate it from some other
input. You would need preprocessor-like statements for generating the
question vs the solution. This would also force the code to acutally
compile. Ok idea is: the main file is in the app folder (we'll just
call the test app which it is, in this case), the submission in
Lib. App imports things from Lib. During test execution submissions
are successively symlinked to that file.


Continuation of above thought: with stack projects we ought to be able
to distinguish between the test and submission dependencies.

TO FIND out: would this work w/out having to mess w/ package.yaml
files every time?

3) Another problem was: If custom datatypes were defined, these were
^-- this should be solved if the ex. solution is also in Lib and
imports from H

defined in the Submission AS WELL AS the target solution. The effect
was that one had to
   a) convert between them (very repetitive thing)
   b) write the solutions in the test file (high coupling)
   c) import data types from the submissions in the solution (have to
   test with mock solutions... that wasn't great.)

2) The python process doing the steps monolithically was horrible bc I
would keep having to restart it (as evidenced by the move10tmp.sh
script). This time around we want a makefile-type setup, though we
don't want to use make bc the target specification feels too
cumbersome to me (having to come up with all the different extensions
was a nightmare).
^-- to prevent redundancy, the generation of output for each submission
for each exercise should be a build target. The problem I had with
make until now was that if you specify a phony target for result
files, they are deleted upon completion.

Continuation: If I saw some unwanted behavior occuring w/ later
submissions, I had to restart the whole process. So maybe we do have
to fix this w/ makefiles?

Continuation again: The upper loop iterated over exercises, not
submissions. So I would only see if an exercise misbehaved after an
entire iteration.

THIS ABSOLUTELY WILL NOT DO.

3) Need to check compatibility with the new upload comment format on
the side of the python script.
^^-- I think this was addressed with the uni2work submissions we had
at the end of last sememster

4) The part a) and b) Submissions was very annoying and problematic,
since very frequently only parts were submitted, leaving the other
part to cause a compilation error. On the other hand, forcing them to
submit separate files is also bad. Need to communicate somehow that
the stuff has to compile?
^^--actually, that is misremembrance. They had undefined in there, it
worked if you separately tested the files. This would be fixed by just
having them as separate excs (I remember we provided the results of the
ex. solution anyway to account for continuation errors. So I just need
to find a way to deal with this in the script, to extract part
submissions from the files.

5) Dealing with instance declarations which might or might not be
there is likewise very problematic. If the student does not need it
for the submission, we should not require it. We should splice in
declarations with template haskell where possible.
^^--I still haven't completely understood if TH is completely
compile-time

6) What was also very annoying: I couldn't test my Quickcheck code
before having submissions to test on bc the way it was built tightly
bound it to the python script. This led to way overlong precorrection
times. Def have to do it differently this time.
^^--again, this should be fixed by having the stack projects

7) Finding the existing tests for the different exercises in the
different folders was always an unnecessary hassle. Preferrably each
exercise would be encapsulated in a database, with the associated test
file and the construction of the worksheets would be just as modular.
^^--this is something that is probably out of my control.

8) The volume of things that would need to be fixed here to me seem to
already warrant an entire BA. Urghhh. Annoying. On the other hand this
is a very applied topic and wouldn't be of any interest to
theoreticians.

9) Sometimes the addition of the module header didn't work.
^^-- this seems to have had to do with language pragmas. They should
prob. just be removed.

10) I always copy-pasted the initial configuration for the test
files. This lead to errors multiple times. Ofc it made the entire
system less modular/ understandable. Maybe work w/ yas templates?
^^--This could be fixed if I create a separate stack project for
TestUtils, turn it into a package (idk how to do that yet, or how to
import local projects? Maybe this is the BA) then import the
functions. TODO: figure out how to do the Smtng.SmElse notation type
things. 

11) The way the python module converts the quickcheck output into the
stuff it actually writes out is a decorator-like pattern which is
still quite cryptic to me. It caused some issues a few times but was
not reason for major headaches. MMM

Continuation of the thought: We didn't ever use the points, conversely
they were quite a nuisance. The question is therefore, if we want to
disregard them in the new process. MMMM

IDEA: The script should symlink to test files in the directory. That
should ensure that only project dependencies are used? Still not sure
what to do abt a/b submissions. Mrrrg

====Verdict=====
Each exercise will be a stack project, with an example solution and a
test. The test will be in the tests directory, the example solution in
the lib. The package.yaml will specify the packages permitted for the
tests and for the submission.

There will be a global TestUtils project where I will write the code
that I would always copy and paste. 

For test execution, individual submissions will be symlinked to the
lib file. To be considered: Should I convert the python setup to a
shake setup?
Essentially a problem is that the directory structure that students
have in their submissions can look a royal mess.

Essentially it would be nicer if exercises got a semantic name, that
way they would be more reusable. But easier for the students to search
for. Anyway Ulli says he wants to rewrite all of them. On the other
hand a mechanism to disambiguate the submissions from their WS-related
titles also seems overly complicated.


=====ssh========

tramp cleanup if broken pipe. The gui /run/etc thingy doesn't work well

now that we've configured the host file it should be much easier to
connect, also to the cip remote! I can't believe I didn't search to
see if there were any solutions to these problems I had (fp)

Take note how many students. 

==Ok for a/b etc exercises we want maybe a map of excs names to list
of subexcs. The specification over the extant files seems slightly
cumbersome. As in, we're extracting information from a reduced form.

===restarting the process===
I think the problem was that the files in the intermediate dir were
overwritten by unwanted results. So actually we would want to create a
'normalized' dir after completing all normalization steps, then copy
that over to the intermediate_dir.

=======old files============
old files should really be moved to a different server as this one
could get nuked by a rogue submission anytime.

======Template Haskell=======
splicing instances conditionally will not work within the test itself,
we have to do it in a separate iteration - all the more reason to turn
this thing into a pipelined process.

===the checker stuff should really go into a repo instead of different
copies strewn all over the place where noone will understand which one
to use. Hmmmm

===We don't want to have to reevaluate things like the costly
structures more than once, on the other hand we don't have to work
with costly persistence. Perhaps running the correction in an
interactive python session will make sense, since I would usually
always watch the shell anyway.

==Ok so student subs to smtng like H1-1a, b will be in one file,
H1-1. The tester should know to look for the test under the full name
but for the submission only under the name without its suffix.

===We don't really need the ex_to_subexp as a map, instead a list of
pairs that just do the splitting up. It's always better to take weaker
prerequisites. That was the idea behind my complicated Typechecker.

===So as not to ruin the grading of previous exercises, results for
all exercises should be written to separate files.

==If we're going to use an interactive shell for this it would be nice
if we could issue a stop command that wouldn't restart the
script. Have to look into that.

==Ok so for normalization: it prob. makes sense to do that always for
all excercises - there should not be a bug in the normalization. Else
we would be put before the needless dillemma whether to iterate over
exercises or submissions. The reason why we need named imports is so
that we can qualify them, that's why we decidedly need the
normalization with the 'module' heading.

==We are going to live with the fact that our iterations go over the
file structure multiple times - it helps designate s.o.c and makes the
individual functions smaller. By the banana split theorem they could
be done in one iteration (only for purely functional procedures
though). It's quite ugly to have iterations for different purposes
cramped into the same loop. That's something the compiler can
optimize, in an expressive enough language. Python doesn't compile in
the first place though... whatevs. Readable code >> slightly faster
execution. Another important note here: When code is together it is
not immediately clear whether it needs to be or happens to be. That is
as I see now so with the normalizations. This is so annoying
though. Now I feel tempted to put the map initialization into the same
function. In Haskell prob. smtng like list fusion would work its
magic. :/

==About the cryptic errors I had with past submissions: Maybe they
were caused by pragmas below the module definition. If that is
actually the case it would probably be best to remove all pragmas.

==Ok: Plan is to read in all the pragmas, remove them, then reinsert
them. Alternatively, we remember the last line we find a pragma at,
then insert the module header below that. If we had semantic parsing
we could distinguish the start of the first code.

# 2022-11-21
`    search_result = re.search(r'^(?:[^\n]+?)\.hs:\d+?:\d+?: error: ?(.+?)^Progress ',stack_build_stderr_output,flags=re.DOTALL | re.MULTILINE)`
^^This doesn't seem to match how compilation errors are output anymore (Progress?). So currently all errors except for bottomless recursion say that the regex didn't match the output. I should have written a test for this, but I didn't 🤪.

[Remove unused external imports · Issue #4 · cxandru/haskell-submission-tester](https://github.com/cxandru/haskell-submission-tester/issues/4)
I'm forced to use a continuation (as opposed to, say, a `do…while` loop) because exceptions are involved in the control flow.

Hmm. I'm adjusting the code to not modify the file directly to better test this. Is that good or is this akin to mocking, akin to what Tom Sydney advocates against?
