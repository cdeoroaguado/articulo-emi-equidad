"""Arma el Excel de la base depurada a partir de los CSV que genera
R/exportar_depurada.R.

Uso:
    python R/armar_excel.py <carpeta_con_csv> <archivo_salida.xlsx>
"""
import sys
from pathlib import Path

import pandas as pd
from openpyxl.styles import Alignment, Font, PatternFill

carpeta, salida = Path(sys.argv[1]), Path(sys.argv[2])
nombres = {
    "1_Notas": "Notas",
    "2_Datos_originales": "Datos_originales",
    "3_Variables_analisis": "Variables_analisis",
    "4_Diccionario": "Diccionario",
    "5_Depuracion": "Depuracion",
    "6_Excluidos": "Excluidos",
}
anchos = {"Notas": {"A": 110}, "Diccionario": {"A": 24, "B": 70, "C": 30, "D": 60},
          "Depuracion": {"A": 8, "B": 95, "C": 10, "D": 14}, "Excluidos": {"A": 10, "B": 65}}

encabezado = Font(bold=True, color="FFFFFF")
relleno = PatternFill("solid", fgColor="1F4E79")

with pd.ExcelWriter(salida, engine="openpyxl") as xw:
    for archivo, hoja in nombres.items():
        df = pd.read_csv(carpeta / f"{archivo}.csv", encoding="utf-8")
        if hoja == "Datos_originales":
            for c in ("Hora de inicio", "Hora de finalización"):
                if c in df:
                    df[c] = pd.to_datetime(df[c])
        df.to_excel(xw, sheet_name=hoja, index=False)
        ws = xw.sheets[hoja]
        ws.freeze_panes = "A2"
        for celda in ws[1]:
            celda.font = encabezado
            celda.fill = relleno
            celda.alignment = Alignment(wrap_text=True, vertical="top")
        if hoja in anchos:
            for col, w in anchos[hoja].items():
                ws.column_dimensions[col].width = w
            for fila in ws.iter_rows(min_row=2):
                for celda in fila:
                    celda.alignment = Alignment(wrap_text=True, vertical="top")
        else:
            for col in ws.columns:
                ws.column_dimensions[col[0].column_letter].width = 18
        if hoja == "Variables_analisis":
            ultima = ws.max_column
            for fila in ws.iter_rows(min_row=2, min_col=ultima, max_col=ultima):
                fila[0].number_format = "0.00"
        if hoja == "Datos_originales":
            for fila in ws.iter_rows(min_row=2, max_col=3):
                for celda in fila:
                    if celda.is_date:
                        celda.number_format = "dd/mm/yyyy hh:mm"

print(f"Guardado: {salida.resolve()}")
