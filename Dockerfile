# Usa una imagen base oficial de Node.js
FROM node:20

# Instala SWI-Prolog
RUN apt-get update && \
    apt-get install -y swi-prolog && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Crea el directorio de trabajo
WORKDIR /app

# Copia todos los archivos al contenedor
COPY . .

# Instala las dependencias
RUN npm install

# Expone el puerto
EXPOSE 3000

# Comando para arrancar la app
CMD ["npm", "start"]
