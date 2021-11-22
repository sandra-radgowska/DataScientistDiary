from bs4 import BeautifulSoup
from requests import get
import sqlite3
from sys import argv

# Applied filters: price (10-20k PLN), manual gearbox, year (2010-2015), Pomeranian Voivodeship

def parse_price(price):
    return float(price.replace(' ', '').replace('zł', '').replace(',', '.'))

def parse_distance(distance):
    return float(distance.replace(' ', '').replace('km', '').replace(',', '.'))

def parse_page(number):
    print(f'I am working on the page number {number}.')
    page = get(f'{URL}&page={number}')
    bs = BeautifulSoup(page.content, 'html.parser')
    for offer in bs.find_all('div', class_='css-1sw7q4x'):
        title = offer.find('p', class_='css-cqgwae-Text eu5v0x0').get_text().strip()
        location = offer.find('p', class_='css-106ejje-Text eu5v0x0').get_text().strip()
        price = parse_price(offer.find('span').get_text().strip())
        year = offer.find('p', class_="css-1obsecn").get_text().strip()[:5]
        distance = parse_distance(offer.find('p', class_="css-1obsecn").get_text().strip()[8:])

        cursor.execute('INSERT INTO offers VALUES (?, ?, ?, ?, ?)', (title, price, location, year, distance))

    db.commit()

URL = 'https://www.olx.pl/d/motoryzacja/samochody/pomorskie/?search%5Bfilter_float_price:from%5D=10000&search%5Bfilter_float_price:to%5D=20000&search%5Bfilter_float_year:from%5D=2010&search%5Bfilter_float_year:to%5D=2015&search%5Bfilter_enum_transmission%5D%5B0%5D=manual'
db = sqlite3.connect('cars_db.db')
cursor = db.cursor()

if len(argv) > 1 and argv[1] == 'setup':
    cursor.execute('''CREATE TABLE offers (title TEXT, price REAL, location TEXT, year TEXT, distance REAL)''')
    quit()

for page in range(1,31):
    parse_page(page)

db.close()