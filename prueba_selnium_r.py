import time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from typing import List, Optional


def get_urls_rel(articulo: str) -> Optional[List[str]]:
    driver = webdriver.Chrome()
    time.sleep(2)

    try:
        driver.get(articulo)

        # aceptación de cookies
        try:
            WebDriverWait(driver, 10).until(
                EC.element_to_be_clickable((By.XPATH, '//*[@id="didomi-notice-agree-button"]'))).click()
        except Exception:
            print("Cookies no encontradas")

        time.sleep(2)

        try:
            body = driver.find_element(By.TAG_NAME, "body")

            while not driver.find_elements(By.XPATH, '//*[@id="article"]/div[2]/div/div/div/div/article/header/h2/a'):
                body.send_keys(Keys.PAGE_DOWN)
                time.sleep(0.1)

            elements = WebDriverWait(driver, 10).until(
                EC.presence_of_all_elements_located((By.XPATH, '//*[@id="article"]/div[2]/div/div/div/div/article/header/h2/a'))
            )

            urls_relacionadas = []
            for element in elements:
                url = element.get_attribute("href")
                urls_relacionadas.append(url)

        except Exception as e:
            print("No hay URLs o ocurrió un error:", str(e))
            return None

        driver.close()

    except Exception as e:
        print("Ha ocurrido un error al acceder al artículo:", str(e))
        return None

    finally:
        # cerramos el driver en incluso en caso de error
        driver.quit()

    return urls_relacionadas

# prueba


urls = get_urls_rel("https://theconversation.com/por-que-tergiversamos-la-informacion-cuando-amenaza-nuestras-creencias-asi-funciona-el-razonamiento-motivado-243555")
print(urls)