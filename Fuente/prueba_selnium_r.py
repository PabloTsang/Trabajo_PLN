#En el caso de que no se tenga selenium instalado
import subprocess
import sys

try:
    import selenium
except ImportError:
    subprocess.check_call([sys.executable, "-m", "pip", "install", "selenium"])
    import selenium
    
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
            return None

        # Manejo de pop-ups
        try:
            if time.time() - start_time > timeout:
                raise TimeoutError("Tiempo límite excedido.")
            popup = WebDriverWait(driver, timeout=2).until(
                EC.element_to_be_clickable((By.XPATH, '//*[@id="__next"]/div[2]/div/div[2]/div/div/div[2]/button/svg'))
            )
            popup.click()
        except Exception:
            pass

        try:
            if time.time() - start_time > timeout:
                raise TimeoutError("Tiempo límite excedido.")
            popup2 = WebDriverWait(driver, timeout=2).until(
                EC.element_to_be_clickable((By.XPATH, '/html/body/div[5]/div[3]/div/div[1]/button'))
            )
            popup2.click()
        except Exception:
            pass

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
            return None

    except TimeoutError as e:
        return None

    except Exception as e:
        return None

    finally:
        # Asegurarse de cerrar el navegador
        driver.quit()

    return urls_relacionadas

