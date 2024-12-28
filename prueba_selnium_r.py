import time
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.keys import Keys
from typing import List, Optional


def get_urls_rel(articulo: str, timeout: int = 60) -> Optional[List[str]]:
    start_time = time.time()  # Marca el inicio de la ejecución
    driver = webdriver.Chrome()

    try:
        driver.get(articulo)

        # Aceptación de cookies
        try:
            if time.time() - start_time > timeout:
                raise TimeoutError("Tiempo límite excedido.")
            WebDriverWait(driver, 10).until(
                EC.element_to_be_clickable((By.XPATH, '//*[@id="didomi-notice-agree-button"]'))
            ).click()
        except Exception:
            print("Cookies no encontradas")

        # Manejo de pop-ups
        try:
            if time.time() - start_time > timeout:
                raise TimeoutError("Tiempo límite excedido.")
            popup = WebDriverWait(driver, timeout=2).until(
                EC.element_to_be_clickable((By.XPATH, '//*[@id="__next"]/div[2]/div/div[2]/div/div/div[2]/button/svg'))
            )
            popup.click()
        except Exception:
            print("No hay popup1")

        try:
            if time.time() - start_time > timeout:
                raise TimeoutError("Tiempo límite excedido.")
            popup2 = WebDriverWait(driver, timeout=2).until(
                EC.element_to_be_clickable((By.XPATH, '/html/body/div[5]/div[3]/div/div[1]/button'))
            )
            popup2.click()
        except Exception:
            print("No hay popup2")

        # Scroll y búsqueda de URLs relacionadas
        try:
            body = driver.find_element(By.TAG_NAME, "body")

            while not driver.find_elements(By.XPATH, '//*[@id="article"]/div[2]/div/div/div/div/article/header/h2/a'):
                if time.time() - start_time > timeout:
                    raise TimeoutError("Tiempo límite excedido.")
                body.send_keys(Keys.PAGE_DOWN)
                time.sleep(0.1)

            elements = WebDriverWait(driver, 10).until(
                EC.presence_of_all_elements_located(
                    (By.XPATH, '//*[@id="article"]/div[2]/div/div/div/div/article/header/h2/a')
                )
            )

            urls_relacionadas = list(set(element.get_attribute("href") for element in elements))

        except Exception as e:
            print("No hay URLs o ocurrió un error:", str(e))
            return None

    except TimeoutError as e:
        print("El proceso tardó más de 60 segundos y fue terminado:", str(e))
        return None

    except Exception as e:
        print("Ha ocurrido un error al acceder al artículo:", str(e))
        return None

    finally:
        # Asegurarse de cerrar el navegador
        driver.quit()

    return urls_relacionadas


# Prueba
urls = get_urls_rel(
    "https://theconversation.com/por-que-tergiversamos-la-informacion-cuando-amenaza-nuestras-creencias-asi-funciona-el-razonamiento-motivado-243555",
    timeout=60
)
print(urls)
