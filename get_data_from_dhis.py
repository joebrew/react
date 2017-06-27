from selenium import webdriver
import time
import os
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options
from shutil import copyfile
import yaml
# os.chdir('/home/joebrew/Documents/react')
with open("credentials/credentials.yaml", 'r') as stream:
    try:
        credentials = yaml.load(stream)
    except yaml.YAMLError as exc:
        print(exc)

driver_loc = "/usr/lib/chromium-browser/chromedriver"
os.environ["webdriver.chrome.driver"] = driver_loc

options = webdriver.ChromeOptions() 
options.add_argument("download.default_directory=/home/joebrew/Documents/zambezia/data")


# Set stuff up
username = credentials['dhis2_username']
password = credentials['dhis2_password']
from selenium import webdriver
driver = webdriver.Chrome(driver_loc, chrome_options = options)

driver.get("https://www.dhis2.org.mz/prod")
driver.implicitly_wait(1)  # seconds

driver.find_element_by_id("j_username").send_keys(username)
driver.find_element_by_id("j_password").send_keys(password)
driver.find_element_by_id("submit").submit()

time.sleep(2)

# Go to pivot tables
driver.get("https://www.dhis2.org.mz/prod/dhis-web-pivot/#")

time.sleep(2)


# Click on the upper left box
# driver.find_element_by_id("ext-gen2057").submit()
# time.sleep(5)
actions = ActionChains(driver) 
actions = actions.send_keys(Keys.TAB)
actions.perform()

# Click down arrow two times
N = 2
actions = ActionChains(driver) 
for _ in range(N):
    actions = actions.send_keys(Keys.DOWN)
actions.perform()

# Press enter
actions = ActionChains(driver) 
actions = actions.send_keys(Keys.RETURN)
actions.perform()

driver.implicitly_wait(5)  # seconds

# Find next button
actions = ActionChains(driver) 
actions = actions.send_keys(Keys.TAB)
actions.perform()

# # Click on the next box down
# driver.find_element_by_id("ext-gen2101").submit()

N = 18 # number of times to press down
actions = ActionChains(driver) 
for _ in range(N):
    actions = actions.send_keys(Keys.DOWN)
actions.perform()

# Press enter
actions = ActionChains(driver) 
actions = actions.send_keys(Keys.RETURN)
actions.perform()

# Wait a few seconds
driver.implicitly_wait(5)  # seconds


# driver.find_element_by_link_text("showExportFormatDialog(&#039;ALL&#039;);")
# driver.execute_script("showExportFormatDialog(&#039;ALL&#039;)")
driver.execute_script("document.getElementsByClassName('ui-button-text')[1].click()")

# Click on csv export
e = driver.find_element_by_id('export_choices_table')
e.click()

from selenium.webdriver.common.action_chains import ActionChains

N = 13  # number of times you want to press TAB

actions = ActionChains(driver) 
for _ in range(N):
    actions = actions.send_keys(Keys.TAB)
actions.perform()

actions = ActionChains(driver) 
actions = actions.send_keys(Keys.RETURN)
actions.perform()

time.sleep(60)

# Specify download location
N = 3  # number of times you want to press TAB

actions = ActionChains(driver) 
for _ in range(N):
    actions = actions.send_keys(Keys.TAB)
actions.perform()


actions = ActionChains(driver) 
actions = actions.send_keys(Keys.RETURN)
actions.perform()

time.sleep(30)

# Get the name of the file
os.chdir('/home/joebrew/Downloads')
max_mtime = 0
for dirname,subdirs,files in os.walk("."):
    for fname in files:
        full_path = os.path.join(dirname, fname)
        mtime = os.stat(full_path).st_mtime
        if mtime > max_mtime:
            max_mtime = mtime
            max_dir = dirname
            max_file = fname

# Move file from downloads
destination_dir = '/home/joebrew/Documents/zambezia/data'
destination_file = destination_dir + '/' + max_file
copyfile(src = max_file, dst = destination_file)

# Print a message
print 'Done. File at ' + destination_file
# time.sleep(50)
# driver.close()