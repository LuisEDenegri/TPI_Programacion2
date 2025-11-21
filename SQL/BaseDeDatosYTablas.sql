-- Creamos la Base de dato correspondiente
CREATE DATABASE DispositivoTAConfiguracionRedB;

-- Usamos la base de dato creada
USE DispositivoTAConfiguracionRedB;

-- Creamos las tablas de entidad A (DisposituvoloT)

CREATE TABLE DispositivoIoT (
id BIGINT PRIMARY KEY AUTO_INCREMENT,
serial VARCHAR(50) NOT NULL UNIQUE,
modelo VARCHAR(50) NOT NULL,
ubicacion VARCHAR(120),
firmware_version VARCHAR(30),
eliminado BOOLEAN NOT NULL DEFAULT FALSE
);

-- Creamos la tabla de entidad B (Configuracion Red)
CREATE TABLE ConfiguracionRed (
id BIGINT PRIMARY KEY AUTO_INCREMENT,
ip VARCHAR(45),
mascara VARCHAR(45),
gateway VARCHAR(45),
dns_primario VARCHAR(45),
dhcp_habilitado BOOLEAN NOT NULL,
eliminado BOOLEAN NOT NULL DEFAULT FALSE,
-- Clave Foránea Única: Asegura la relación 1:1.
-- Cada ConfiguracionRed pertenece a un y solo un DispositivoIoT.
dispositivo_id BIGINT NOT NULL UNIQUE,
FOREIGN KEY (dispositivo_id) REFERENCES DispositivoIoT(id)
ON DELETE CASCADE -- Elimina B si se elimina A físicamente
);

-- Hacemos Scrip para datos de prueba (INSERT)

-- Insertar Dispositivo IoT (A)
INSERT INTO DispositivoIoT (serial, modelo, ubicacion, firmware_version)
VALUES ('S2024-001', 'SensorTemp-v2', 'Almacén 1', '2.1.0');

-- Insertar Configuración de Red (B) para el Dispositivo (id=1)
INSERT INTO ConfiguracionRed (ip, mascara, gateway, dns_primario, dhcp_habilitado, dispositivo_id)
VALUES ('192.168.1.10', '255.255.255.0', '192.168.1.1', '8.8.8.8', FALSE, 1);

-- Insertar otro Dispositivo IoT (A)
INSERT INTO DispositivoIoT (serial, modelo, ubicacion, firmware_version)
VALUES ('S2024-002', 'Camara-v1', 'Oficina Principal', '1.5.3');