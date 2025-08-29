library(sf)
"../../../Repositorios/Municipal_Inversion_3_años/Datos/SIPDUS_INHIFE.geojson" |> st_read()->obras_c_inversion

inversion_p_municipio_periodo=obras_c_inversion |> st_drop_geometry() |> dplyr::group_by(Municipio) |> 
  dplyr::summarise(inversion=sum(Inversión,na.rm=T))
inversion_p_municipio_año=obras_c_inversion |> st_drop_geometry() |> dplyr::group_by(Ejercicio,Municipio) |> 
  dplyr::summarise(inversion=sum(Inversión,na.rm=T))

inversion_p_municipio_periodo=inversion_p_municipio_periodo |> 
  dplyr::filter(Municipio!='Varios')


municipios=read_sf("../../../Reutilizables/Cartografia/LIM_MUNICIPALES.shp")

"../../../Repositorios/Seguridad_Tablero_Movil/Municipal-Delitos - Julio 2025 (2015-2025).csv" |> read.csv(check.names = F,fileEncoding = "latin1") ->delitos_p_mun
delitos_p_mun=delitos_p_mun |> 
  dplyr::filter(Año%in%c(2022:2025),Entidad=='Hidalgo')

delitos_p_mun=delitos_p_mun |> dplyr::select(Año,Municipio,Enero:Diciembre) |> 
  dplyr::group_by(Año,Municipio) |> 
  dplyr::summarise_all(\(x){sum(x,na.rm = T)})

delitos_p_mun$Total=rowSums(delitos_p_mun |> dplyr::ungroup() |> dplyr::select(Enero:Diciembre))

delitos_p_mun_año=delitos_p_mun |> 
  dplyr::group_by(Año,Municipio) |> 
  dplyr::summarise(Total=sum(Total,na.rm=T))

delitos_p_mun_periodo=delitos_p_mun |> 
  dplyr::group_by(Municipio) |> 
  dplyr::summarise(Total=sum(Total,na.rm=T))


################################
intercensal_mun_2020=readxl::read_excel("../../../Repositorios/Seguridad_Tablero_Movil/Estructura_Datos/Banco de datos infografias _Eduardo.xlsx")
intercensal_mun_2020=intercensal_mun_2020|>
  dplyr::select(Municipio,`Población total`)|>
  dplyr::filter(!is.na(Municipio)& Municipio!='Estatal')

delitos_p_mun_periodo=merge(delitos_p_mun_periodo,intercensal_mun_2020,by='Municipio',all.x=T)
delitos_p_mun_periodo=delitos_p_mun_periodo |> 
  dplyr::mutate(tasa_p_c_mil=1000*(Total/as.numeric(`Población total`))) |> 
  dplyr::select(Municipio,tasa_p_c_mil)
delitos_e_inversion_periodo=delitos_p_mun_periodo |> merge(inversion_p_municipio_periodo,by='Municipio',all.x=T)

delitos_e_inversion_periodo_c_geom=delitos_e_inversion_periodo |> merge(municipios |> dplyr::select(NOM_MUN),by.x='Municipio',by.y='NOM_MUN',all.x=T) |> 
  st_as_sf()|> st_set_crs("EPSG:32614")

library(viridis)
par(mfrow=c(2, 1)) 
library(sf)

plot(delitos_e_inversion_periodo_c_geom["tasa_p_c_mil"], main = "Tasa de Delitos", pal = viridis)

# Mapa 2: Inversión
plot(delitos_e_inversion_periodo_c_geom["inversion"], main = "Inversión", pal = viridis)
cor.test(delitos_e_inversion_periodo$tasa_p_c_mil,delitos_e_inversion_periodo$inversion)
