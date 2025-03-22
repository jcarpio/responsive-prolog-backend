# Imagen base con Node
FROM node:20

# Instalamos SWI-Prolog
RUN apt-get update && \
    apt-get install -y swi-prolog && \
    apt-get clean

# Creamos directorio de trabajo
WORKDIR /app

# Copiamos el código
COPY . .

# Instalamos dependencias
RUN npm install

# Exponemos el puerto
EXPOSE 3000

# Comando para arrancar la app
CMD ["npm", "start"]
