# -*- coding: utf-8 -*-
"""
Second Steps with Python
StatLab
W. D. Brinda
"""


########################################################################
########### Part 1: Building on topics from "First Steps" ##############
########################################################################





### Tuples

# A tuple is like a "lightweight" list
# - computations with tuples are generally faster and less memory intensive
# - but tuples are less versatile

# use parentheses rather than square brackets
t = (1, 2, 8)
t

# or just separate the entries with commas
u = 4, 5, 9
u

# you can even use a tuple on both sides of the equal sign
# for entry-by-entry assignment
v, w, x = 3, ["hi", "there"], range(5)
v
w
x

# here's a function that returns a tuple
def squareAndCube(x):
    return x**2, x**3

a, b = squareAndCube(3)
a
b


# entries and subsets
u[0]
u[1:]

# changing a tuple?
u[0] = 1 # doesn't work! unlike lists

# can turn a tuple into a list
list(u)


### List comprehension

# Suppose you want to make a new list by doing the same thing to each entry of another list.
# As a simple example, let's create a list of the squares of 1 through 10
squares = []
for i in range(1, 11):
    squares.append(i**2)
squares

# Python has a special shorthand notation for this type of task,
# with the for loop inside the list definition.
[i**2 for i in range(1, 11)]

# You can also add a conditional in order to only include a subset of the list.
# For example, here are the squares of the even numbers between 1 and 10.
[i**2 for i in range(1, 11) if i%2==0]

# Realize that the this isn't limited to lists of numbers
greeting = "What are you going to do today"
# List of the words that start with "T" or "t"
[s for s in greeting.split() if s[0] in ["T", "t"]]
# Make them all-capitals
[s.upper() for s in greeting.split() if s[0] in ["T", "t"]]

# List comprehension isn't strictly necessary, but it does allow you to pack
# quite a bit of work in a single tidy line of code.

## EXERCISE: In a single line of code, create a list of the words in the
##           "greeting" string defined above that have at least four letters.
##           The words should be converted to all-lower-case letters.

[s.lower() for s in greeting.split() if len(s) >= 4]










### String manipulation

declaration = """The unanimous Declaration of the thirteen united States of America, When in the Course of human events, it becomes necessary for one people to dissolve the political bands which have connected them with another, and to assume among the powers of the earth, the separate and equal station to which the Laws of Nature and of Nature's God entitle them, a decent respect to the opinions of mankind requires that they should declare the causes which impel them to the separation.
We hold these truths to be self-evident, that all men are created equal, that they are endowed by their Creator with certain unalienable Rights, that among these are Life, Liberty and the pursuit of Happiness.--That to secure these rights, Governments are instituted among Men, deriving their just powers from the consent of the governed, --That whenever any Form of Government becomes destructive of these ends, it is the Right of the People to alter or to abolish it, and to institute new Government, laying its foundation on such principles and organizing its powers in such form, as to them shall seem most likely to effect their Safety and Happiness. Prudence, indeed, will dictate that Governments long established should not be changed for light and transient causes; and accordingly all experience hath shewn, that mankind are more disposed to suffer, while evils are sufferable, than to right themselves by abolishing the forms to which they are accustomed. But when a long train of abuses and usurpations, pursuing invariably the same Object evinces a design to reduce them under absolute Despotism, it is their right, it is their duty, to throw off such Government, and to provide new Guards for their future security.--Such has been the patient sufferance of these Colonies; and such is now the necessity which constrains them to alter their former Systems of Government. The history of the present King of Great Britain is a history of repeated injuries and usurpations, all having in direct object the establishment of an absolute Tyranny over these States. To prove this, let Facts be submitted to a candid world.
He has refused his Assent to Laws, the most wholesome and necessary for the public good.
He has forbidden his Governors to pass Laws of immediate and pressing importance, unless suspended in their operation till his Assent should be obtained; and when so suspended, he has utterly neglected to attend to them.
He has refused to pass other Laws for the accommodation of large districts of people, unless those people would relinquish the right of Representation in the Legislature, a right inestimable to them and formidable to tyrants only.
He has called together legislative bodies at places unusual, uncomfortable, and distant from the depository of their public Records, for the sole purpose of fatiguing them into compliance with his measures.
He has dissolved Representative Houses repeatedly, for opposing with manly firmness his invasions on the rights of the people.
He has refused for a long time, after such dissolutions, to cause others to be elected; whereby the Legislative powers, incapable of Annihilation, have returned to the People at large for their exercise; the State remaining in the mean time exposed to all the dangers of invasion from without, and convulsions within.
He has endeavoured to prevent the population of these States; for that purpose obstructing the Laws for Naturalization of Foreigners; refusing to pass others to encourage their migrations hither, and raising the conditions of new Appropriations of Lands.
He has obstructed the Administration of Justice, by refusing his Assent to Laws for establishing Judiciary powers.
He has made Judges dependent on his Will alone, for the tenure of their offices, and the amount and payment of their salaries.
He has erected a multitude of New Offices, and sent hither swarms of Officers to harrass our people, and eat out their substance.
He has kept among us, in times of peace, Standing Armies without the Consent of our legislatures.
He has affected to render the Military independent of and superior to the Civil power.
He has combined with others to subject us to a jurisdiction foreign to our constitution, and unacknowledged by our laws; giving his Assent to their Acts of pretended Legislation:
For Quartering large bodies of armed troops among us:
For protecting them, by a mock Trial, from punishment for any Murders which they should commit on the Inhabitants of these States:
For cutting off our Trade with all parts of the world:
For imposing Taxes on us without our Consent:
For depriving us in many cases, of the benefits of Trial by Jury:
For transporting us beyond Seas to be tried for pretended offences
For abolishing the free System of English Laws in a neighbouring Province, establishing therein an Arbitrary government, and enlarging its Boundaries so as to render it at once an example and fit instrument for introducing the same absolute rule into these Colonies:
For taking away our Charters, abolishing our most valuable Laws, and altering fundamentally the Forms of our Governments:
For suspending our own Legislatures, and declaring themselves invested with power to legislate for us in all cases whatsoever.
He has abdicated Government here, by declaring us out of his Protection and waging War against us.
He has plundered our seas, ravaged our Coasts, burnt our towns, and destroyed the lives of our people.
He is at this time transporting large Armies of foreign Mercenaries to compleat the works of death, desolation and tyranny, already begun with circumstances of Cruelty & perfidy scarcely paralleled in the most barbarous ages, and totally unworthy the Head of a civilized nation.
He has constrained our fellow Citizens taken Captive on the high Seas to bear Arms against their Country, to become the executioners of their friends and Brethren, or to fall themselves by their Hands.
He has excited domestic insurrections amongst us, and has endeavoured to bring on the inhabitants of our frontiers, the merciless Indian Savages, whose known rule of warfare, is an undistinguished destruction of all ages, sexes and conditions.
In every stage of these Oppressions We have Petitioned for Redress in the most humble terms: Our repeated Petitions have been answered only by repeated injury. A Prince whose character is thus marked by every act which may define a Tyrant, is unfit to be the ruler of a free people.
Nor have We been wanting in attentions to our Brittish brethren. We have warned them from time to time of attempts by their legislature to extend an unwarrantable jurisdiction over us. We have reminded them of the circumstances of our emigration and settlement here. We have appealed to their native justice and magnanimity, and we have conjured them by the ties of our common kindred to disavow these usurpations, which, would inevitably interrupt our connections and correspondence. They too have been deaf to the voice of justice and of consanguinity. We must, therefore, acquiesce in the necessity, which denounces our Separation, and hold them, as we hold the rest of mankind, Enemies in War, in Peace Friends.
We, therefore, the Representatives of the united States of America, in General Congress, Assembled, appealing to the Supreme Judge of the world for the rectitude of our intentions, do, in the Name, and by Authority of the good People of these Colonies, solemnly publish and declare, That these United Colonies are, and of Right ought to be Free and Independent States; that they are Absolved from all Allegiance to the British Crown, and that all political connection between them and the State of Great Britain, is and ought to be totally dissolved; and that as Free and Independent States, they have full Power to levy War, conclude Peace, contract Alliances, establish Commerce, and to do all other Acts and Things which Independent States may of right do. And for the support of this Declaration, with a firm reliance on the protection of divine Providence, we mutually pledge to each other our Lives, our Fortunes and our sacred Honor."""

# Count the number of occurrences of a substring within a larger string
declaration.count(" the ")
declaration.count("The ")

# Finding the first occurrence of a substring within larger string
index = declaration.find("Quartering")
index
declaration[index:(index+100)]

# Replacing all occurrences of a substring within a larger string
new_declaration = declaration.replace("united", "divided")
new_declaration[:100]
# if you only want to replace the first occurrence in the string: declaration.replace("united", "divided", 1)

# Splitting a string into a list of strings at every new-line (usually encoded as "\n")
lines = declaration.splitlines()
lines[0] # first line
lines[-1] # last line

# Putting a list of strings together into a single string
s = "\n".join(lines)
s == declaration

# There are many other str functions that can be found in the Python
# documentation and tutorials online.


## EXERCISE: Create a string of the declaration of independence that has all
##           occurrences of "We" or "we" in the last paragraph
##           replaced with all-caps "WE".

lines = new_declaration.splitlines()
s = lines[-1]
s.replace("We", "WE")
s.replace("we", "WE")
lines[-1] = s
"\n".join(lines)







## EXERCISE: Use list comprehension to create a list of strings
##           ["declaration0", "declaration1", ... ]
##           one entry for each line in the declaration.
##           i.e. there are 32 lines, so the last entry of your
##           list should be "declaration31".


[f"declaration{ i }" for i in range(len(lines))]






### Regular expressions

# The "find" and the "replace" functions work well when you have a
# specific substring in mind. But what if you wanted to, for example,
# find every word that begins with a capital letter?
# Many tasks that are difficult or impossible with the ordinary string functions become
# quite easy if you have enough proficiency with "regular expressions".


import re

s = "What do you think you'll do this year?"

# Find every instance of "you" - nothing special going on here
re.findall("you", s)

# Now let's start to see how regular expressions provide more flexibility.
# The following command finds all substrings that have a y followed by
# any number of letters.
re.findall("y\w*", s)
# The "\w" matches any word character (upper- or lower-case letter, apostrophe, hyphen)
# while the "*" means any number of occurrences of such.

re.findall("y\w*", "The very same things I do every year.")
re.findall("y\w+", "The very same things I do every year.")
# The "+" means at least one occurrence.

# Now let's find every word that has at least one "y".
# That means any number of word characters,
# followed by a y, followed by any number of word characters.
re.findall("\w*y\w*", "The very same things I do every year.")

# Let's find all the words that start with a capital letter.
re.findall("[A-Z]\w*", declaration)

# Let's consider occurrences of the word "other" or "others" (lower-case only)
re.findall("other", declaration)
# Upon closer inspection, one of these matches is actually "another"
re.findall("\w*other\w*", declaration)
# We can make sure that "other" is at the beginning of the word.
re.findall("\\bother\w*", declaration)
# The "\\b" matches a word boundary. https://developmentality.wordpress.com/2011/09/22/python-gotcha-word-boundaries-in-regular-expressions/
# But this regular expression would still match, for example, "otherness"
re.findall("\\bothers?", declaration)
# The "?" means either zero or one occurrences of the preceding "s".

# Find all occurrences of "We" or "we"
re.findall("\\b[Ww]e\\b", declaration)


# --------
# We'll probably skip the rest of this regular expressions section
# during the workshop, but you can use it for your future reference.
# --------


# Find all occurrences of States or Colonies
re.findall("States\\b|Colonies\\b", declaration)
# The "|" means OR.

# Let's find all the paragraphs that start with "He has"
re.findall("He has .*", declaration)
# The "." matches everything that isn't a newline character
# This isn't ideal because it could match "He has" starting anywhere
# within a paragraph. To make sure we're starting at the beginning:
re.findall("^He has .*", declaration, re.MULTILINE)
# The "^" matches the beginning of the string, and
# with re.MULTILINE, it also matches the beginning of any line.

# Similarly, "$" matches the end of the string (or of any line when MULTILINE is specified).
re.findall("Hands\.$", declaration, re.MULTILINE)
# Notice that "\." was needed to match the period "."
# The backslash "escapes" the ordinary treatment of "." in a regular expression.

# You can use parentheses to indicate the part(s) of the regular expression
# that you want to "keep".
re.findall("^He has (.*)", declaration, re.MULTILINE)
re.findall("^He has (\w*) .*", declaration, re.MULTILINE)


# You can also use regular expressions to identify substrings that you
# want to replace. The "re.sub()" function from the re module does the trick.
# Let's replace every "We" and "we" with "WE".
new_declaration = re.sub("[Ww]e\\b", "WE", declaration)
new_declaration[-100:]


# You can keep track of specific portions of a matched substring using "re.search()"
s = lines[-1]
s
r = re.search("we mutually pledge to each other our (.*), our (.*) and our (.*)\.", s)
r.groups()
r.groups()[0]

# the search object can also be used as a conditional
# indicating whether or not a match was found within the string
if r:
    print("match found")

# Regular expressions are incredibly powerful, but they can be challenging to use.
# Much more documentation is available on the web.
# https://www.w3schools.com/python/python_regex.asp
# https://www.tutorialspoint.com/python/python_reg_expressions.htm
# https://docs.python.org/3/library/re.html


## EXERCISE: Create a string of the declaration of independence in which
##           the word after "He has" is written in all capitals in each of the
##           "He has"-paragraphs.
##           Hint: use a for loop and do re.search() on each line of the Declaration.


for i in range(len(lines)):
    s = re.search("^He has (\w+) .*", lines[i])
    if s:
        lines[i] = lines[i].replace(s.group(1), s.group(1).upper(), 1)
new_declaration = "\n".join(lines)

# check:
re.findall("^He has .*", new_declaration, re.MULTILINE)














### File manipulation

import os
os.getcwd() # what directory are we currently working in?

from pathlib import Path

# Let's write the Declaration as a text file.
dec_file = Path("declaration.txt")
dec_file.write_text(declaration)

# Let's read in the file we just made
d = dec_file.read_text()

# Is what we read in from the file identical to what we wrote to the file?
d==declaration

# Let's write each paragraph of the Declaration into its own text file

# first we'll make a new directory to store them in
dec_dir = Path("declaration-lines")
dec_dir.mkdir()

for i in range(len(lines)):
    (dec_dir / f"declaration{ i }.txt").write_text(lines[i])

# Where did Python write all the files we've made?
os.getcwd() # to refresh your memory
# check on your own computer to see them
# (look for the "declaration-lines" directory)


# Let's open and read each of those text files
p = []
for i in range(len(lines)):
    line = (dec_dir / f"declaration{ i }.txt").read_text()
    p.append(line)

# Is the list that we read in from the files
# identical to the list we wrote to the files?
p==lines


# Let's construct a (pointless) dataset and save it as a csv file.
# For each paragraph, we're going to find the total number of words
# and the number of times "The" or "the" appeared.
num_words = [None]*len(lines) # initialze list of Nones
num_the = [None]*len(lines)
for i in range(len(lines)):
    num_words[i] = len(lines[i].split()) # number of words in this paragraph
    num_the[i] = len(re.findall("\\b[Tt]he\\b", lines[i])) # number of "The" or "the"

# Each paragraph of the declaration will get a row of the csv file.
# Rather than using write_lines repeatedly, we'll keep the file open
# while iterating through the paragraphs.
csv_file = Path("declaration.csv")
with csv_file.open("w") as f:
    f.write("num_words, num_the\n") # write a header line giving column names
    for i in range(len(lines)):
        f.write(f"{ num_words[i] }, { num_the[i] }\n")


# Let's move the csv file to a new directory (and rename it).
# First, we'll make another new directory
new_dir = Path("statlab-python-files/second-steps")
new_dir.mkdir(parents=True)
# use parents=True if the directory has parent directories
# that also need to be created
csv_target = new_dir / "declaration-wordcounts.csv"
csv_file.rename(csv_target)



# Recall how to get the paths of the files of a given filetype
dec_dir.glob("*.txt")
list(dec_dir.glob("*.txt"))


## EXERCISE: Write a function concatenateTxtFiles
##           that takes a directory path as its first argument
##           and a file path as its second argument. It should read in
##           all of the .txt files in the directory provided, and put their
##           text together (separated by newline characters "\n")
##           into a new file, saved as the file path provided.
##           Try it out with the arguments dec_dir and new_file.
##           (It's okay if the declaration paragraphs aren't put back
##           together in the correct order.)

new_file = Path("declaration_together.txt")

def concatenateTxtFiles(dir_path, file_path):
    full = "\n".join([file.read_text() for file in dir_path.glob("*.txt")])
    file_path.write_text(full)

concatenateTxtFiles(dec_dir, new_file)









# Finally, let's clean up by deleting the files and folders we made
new_file.unlink()
dec_file.unlink()
for i in range(len(lines)):
    (dec_dir / f"declaration{ i }.txt").unlink()
dec_dir.rmdir()
csv_target.unlink()
new_dir.rmdir()
os.rmdir("statlab-python-files") # or Path("statlab-python-files").rmdir()

# Note that rmdir only works on empty directories.
# There is a function for deleting non-empty directories, but I'm not
# including it here because it's very dangerous for beginners!
# You would need to make sure you know exactly what you're doing
# or you can accidentally delete a lot more than you were planning to.

















### System commands

# Let's send a command to the command line
# (a.k.a. the "Command Prompt" in Windows or the "Terminal" in Mac/Linux)
# e.g. Run the following if using Windows:
os.system("notepad")
# Many programs can be run via the command line, so "os.system()"
# is a powerful part of automating complicated processes with Python.





### Running a Python script


# You can also run a Python script from the command line.
# This can be a little complicated to set up, and it varies from system to system
# (e.g. I had to add "C:\Users\12565\Anaconda3" to my PATH variable).
# So we won't do this together as part of the workshop.
# But you should know that it's possible and see a couple of examples.

# Suppose the file called "run.py" contains only the following line.
print("It works!")

# If I open Command Prompt, change my working directory to wherever I've
# stored "run.py", and type "python run.py", it runs the python code
# in that file. I see the text "It works!" printed in my command prompt.

# Now instead, suppose "run.py" contains the following two lines.
import sys
sys.stdout.write("Hi, %s. It works! %s" % (sys.argv[1], sys.argv[2]))

# Then I can pass arguments to the script.
# For example, "python run.py David Nice!" results in the output
# Hi, David. It works! Nice!
# The sys.stdout.write() function passes text back to the command prompt
# in such a way that it can be "piped" into another program if desired.

# If you'd like to get this set up on your own system, you can talk
# to the workshop instructor and/or find information online.





### Package management

# In addition to the packages we've seen, such as re, os, and sys, there are
# countless others that also extend the functionality of the Python language.
# If you installed Python via Anaconda Distribution, several thousand
# such packages are listed in Environments tab of Anaconda Navigator.
# With the click of a button, any chosen package will be downloaded
# and installed; then all of the functions and objects defined by that package
# become available to you with a single "import" command.

# In this same Environments tab, you can also update any package for which
# a newer version has been released.
# Realize that it's possible (though uncommon) that code you've already
# written may actually stop working once you update a package.
# To prevent this issue, you can look into using "virtual environments",
# but that's beyond the scope of this workshop.








########################################################################
############################ Part 2: Classes ###########################
########################################################################


# By creating a class, you're able to define a new type of variable
# with its own properties and functions.

# Defining a class
class YaleCourse:
    
    def __init__(self, department, number, credits):
        self.department = department
        self.number = number
        self.credits = credits


# Creating instances of this class
c1 = YaleCourse("S&DS", 100, 3)
c2 = YaleCourse("S&DS", 312, 3)
# (If there is a function called __init__, it runs automatically
#  whenever a new instance of the class is created.
#  The "self" parameter refers to the specific instance at hand,
#  and it's passed into the function automatically.)

# The __init__ function created class properties for each YaleCourse instance that we defined.
# The properties can be accessed by instance-name dot property-name.
c1.number
c2.number


# Defining another class
class YaleStudent:    

    def __init__(self, name):
        self.name = name
        self.courses = []
        self.credits = 0
    
    def courseComplete(self, course):
        self.courses.append(course)
        self.credits = self.credits + course.credits

# Creating an instance of this class
s = YaleStudent("Jack")
s.credits

# A class function (or "method") is called by instance-name dot property-name.
# The instance is automatically passed as the first argument, called "self".
s.courseComplete(c1)
s.credits
s.courseComplete(c2)
s.credits

# Loop through the list of completed courses (each is a YaleCourse object)
for c in s.courses:
    print(c.department, "-", c.number)


# Even if you never create your own classes, understanding this material will
# go a long way toward helping you make sense of why Python code looks
# the way that it does.
# For example, strings in Python are actually instances of a class called "str".
# Quotation marks are a shortcut for creating strings, but you do sometimes
# explicitly use "str" to construct a string from another type of data.

z = str(3.14**2)
z

# "find", "replace", and "split" are just a few of the functions defined for
# the str class, so they can be called on "z" using the dot notation.
z.split('.')


## EXERCISE: Define another class called "YaleFaculty". It's __init__ function
##           should take the faculty member's name and a list of YaleCourses
##           that the faculty member teaches (an empty list by default).
##           An additional class function should be written that appends
##           a given YaleCourse to the faculty member's list
##           unless it is already on the list.


class YaleFaculty:

    def __init__(self, name, courses=[]):
        self.name = name
        self.courses = courses
    
    def addCourse(self, course):
        if course not in self.courses:
            self.courses.append(course)





