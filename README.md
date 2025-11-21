# Sistema de Gestión de Dispositivos IoT

## 📋 Descripción del Dominio

Este proyecto implementa un **Sistema de Gestión de Dispositivos IoT con Configuración de Red** utilizando Java y JDBC. El sistema maneja una relación **1→1 unidireccional** entre DispositivoIoT (Entidad A) y ConfiguraciónRed (Entidad B), garantizando integridad referencial y transaccionalidad mediante operaciones CRUD completas.

### Características Principales

- ✅ Operaciones CRUD transaccionales (Create, Read, Update, Delete)
- ✅ Baja lógica de registros (soft delete)
- ✅ Validaciones de negocio robustas
- ✅ Gestión de relaciones 1→1 con integridad referencial
- ✅ Arquitectura en capas (Presentación → Servicio → DAO → Base de Datos)
- ✅ Manejo de excepciones personalizadas
- ✅ Transacciones ACID con commit/rollback automático

---

## 🗂️ Estructura del Proyecto

```
proyecto-iot/
├── src/
│   ├── main/
│   │   └── AppMenu.java                    # Interfaz de usuario (menú interactivo)
│   ├── service/
│   │   ├── GenericService.java             # Interfaz genérica de servicios
│   │   ├── DispositivoIotService.java      # Lógica de negocio para DispositivoIoT
│   │   └── ConfiguracionRedService.java    # Lógica de negocio para ConfiguracionRed
│   ├── dao/
│   │   ├── DispositivoIotDao.java          # Acceso a datos de DispositivoIoT
│   │   └── ConfiguracionRedDao.java        # Acceso a datos de ConfiguracionRed
│   ├── entities/
│   │   ├── DispositivoIoT.java             # Modelo de datos DispositivoIoT
│   │   └── ConfiguracionRed.java           # Modelo de datos ConfiguracionRed
│   ├── config/
│   │   └── DatabaseConnection.java         # Gestión de conexiones a BD
│   └── exceptions/
│       ├── ServiceException.java           # Excepción para errores de servicio
│       └── ValidationException.java        # Excepción para errores de validación
├── sql/
│   ├── schema.sql                          # Script de creación de base de datos y tablas
│   └── data.sql                            # Datos de prueba
├── docs/
│   └── diagrama_uml.png                    # Diagrama de clases UML
└── README.md                               # Este archivo
```

---

## 🛠️ Requisitos del Sistema

### Software Necesario

- **Java**: JDK 8 o superior
- **Base de Datos**: MySQL 8.0+ / PostgreSQL 13+ / Oracle 11g+
- **Driver JDBC**: Según la base de datos elegida
  - MySQL: `mysql-connector-java-8.0.x.jar`
  - PostgreSQL: `postgresql-42.x.x.jar`
  - Oracle: `ojdbc8.jar`
- **IDE Recomendado**: Eclipse, IntelliJ IDEA, NetBeans o VS Code

### Dependencias Maven (Opcional)

Si usas Maven, agrega en tu `pom.xml`:

```xml
<dependencies>
    <!-- MySQL -->
    <dependency>
        <groupId>mysql</groupId>
        <artifactId>mysql-connector-java</artifactId>
        <version>8.0.33</version>
    </dependency>
    
    <!-- PostgreSQL (alternativo) -->
    <!-- 
    <dependency>
        <groupId>org.postgresql</groupId>
        <artifactId>postgresql</artifactId>
        <version>42.6.0</version>
    </dependency>
    -->
</dependencies>
```

---

## 🗄️ Configuración de la Base de Datos

### Paso 1: Crear la Base de Datos

Ejecuta el script `sql/schema.sql` que contiene las instrucciones SQL para crear la base de datos y las tablas necesarias.

#### Para MySQL:

```sql
-- Crear base de datos
CREATE DATABASE IF NOT EXISTS iot_system 
CHARACTER SET utf8mb4 
COLLATE utf8mb4_unicode_ci;

USE iot_system;

-- Tabla ConfiguracionRed (Entidad B)
CREATE TABLE configuracion_red (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    dhcp_habilitado BOOLEAN NOT NULL,
    ip VARCHAR(45),
    mascara VARCHAR(45),
    gateway VARCHAR(45),
    dns_primario VARCHAR(45),
    eliminado BOOLEAN DEFAULT FALSE,
    fecha_creacion TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    fecha_modificacion TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
) ENGINE=InnoDB;

-- Tabla DispositivoIoT (Entidad A)
CREATE TABLE dispositivo_iot (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    serial VARCHAR(50) UNIQUE NOT NULL,
    modelo VARCHAR(50) NOT NULL,
    ubicacion VARCHAR(120),
    firmware_version VARCHAR(30),
    eliminado BOOLEAN DEFAULT FALSE,
    configuracion_red_id BIGINT UNIQUE,
    fecha_creacion TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    fecha_modificacion TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    CONSTRAINT fk_config_red FOREIGN KEY (configuracion_red_id) 
        REFERENCES configuracion_red(id) 
        ON DELETE SET NULL
) ENGINE=InnoDB;

-- Índices para optimización
CREATE INDEX idx_serial ON dispositivo_iot(serial);
CREATE INDEX idx_modelo ON dispositivo_iot(modelo);
CREATE INDEX idx_eliminado ON dispositivo_iot(eliminado);
```

### Paso 2: Insertar Datos de Prueba

Ejecuta el script `sql/data.sql` para cargar datos de ejemplo:

```sql
-- Insertar configuraciones de red
INSERT INTO configuracion_red (dhcp_habilitado, ip, mascara, gateway, dns_primario) VALUES
(TRUE, NULL, NULL, NULL, NULL),
(FALSE, '192.168.1.100', '255.255.255.0', '192.168.1.1', '8.8.8.8'),
(FALSE, '10.0.0.50', '255.255.0.0', '10.0.0.1', '1.1.1.1');

-- Insertar dispositivos IoT
INSERT INTO dispositivo_iot (serial, modelo, ubicacion, firmware_version, configuracion_red_id) VALUES
('S2024-001', 'SmartSensor-X1', 'Sala Principal', 'v1.2.3', 1),
('S2024-002', 'TempHumidity-Pro', 'Almacén A', 'v2.0.1', 2),
('S2024-003', 'MotionDetector-Plus', 'Entrada Norte', 'v1.5.0', 3);
```

### Paso 3: Configurar la Conexión en el Código

Edita el archivo `src/config/DatabaseConnection.java` con tus credenciales:

```java
private static final String URL = "jdbc:mysql://localhost:3306/iot_system";
private static final String USER = "tu_usuario";
private static final String PASSWORD = "tu_contraseña";
```

---

## ⚙️ Compilación y Ejecución

### Opción 1: Compilación Manual (Línea de Comandos)

#### 1. Compilar el proyecto

```bash
# Navegar al directorio del proyecto
cd proyecto-iot

# Crear directorio para los .class
mkdir -p bin

# Compilar todos los archivos .java
javac -d bin -cp "lib/*" src/**/*.java
```

#### 2. Ejecutar la aplicación

```bash
# Ejecutar el menú principal
java -cp "bin:lib/*" main.AppMenu
```

**Nota para Windows**: Usa punto y coma (`;`) en lugar de dos puntos (`:`)
```cmd
java -cp "bin;lib/*" main.AppMenu
```

---

### Opción 2: Usando IDE (Eclipse/IntelliJ)

#### En Eclipse:

1. **Importar el proyecto**:
   - `File` → `Open Projects from File System`
   - Seleccionar la carpeta del proyecto

2. **Agregar el driver JDBC**:
   - Click derecho en el proyecto → `Build Path` → `Configure Build Path`
   - `Libraries` → `Add External JARs`
   - Seleccionar el archivo `.jar` del driver JDBC

3. **Ejecutar**:
   - Click derecho en `AppMenu.java` → `Run As` → `Java Application`

#### En IntelliJ IDEA:

1. **Abrir el proyecto**:
   - `File` → `Open` → Seleccionar carpeta del proyecto

2. **Agregar dependencias**:
   - `File` → `Project Structure` → `Libraries`
   - `+` → `Java` → Seleccionar el `.jar` del driver JDBC

3. **Ejecutar**:
   - Click derecho en `AppMenu.java` → `Run 'AppMenu.main()'`

---

### Opción 3: Usando Maven

Si tienes configurado `pom.xml`:

```bash
# Compilar
mvn clean compile

# Ejecutar
mvn exec:java -Dexec.mainClass="main.AppMenu"

# Empaquetar JAR
mvn clean package
java -jar target/iot-system-1.0.jar
```

---

## 🎮 Flujo de Uso del Sistema

### Menú Principal

```
--- Menú DispositivoIoT y ConfiguraciónRed ---
1. Crear DispositivoIoT (transaccional)
2. Leer DispositivoIoT por ID
3. Listar todos los DispositivosIoT (incluye eliminados lógicamente)
4. Actualizar DispositivoIoT
5. Eliminar (Baja Lógica) DispositivoIoT
6. Buscar por Serial (campo relevante)
0. Salir
Seleccione una opción: _
```

### Ejemplo de Uso: Crear un Dispositivo

```
Seleccione una opción: 1

Serial (único): S2024-004
Modelo: SmartCamera-HD
Ubicación: Estacionamiento
Firmware: v3.1.0

¿DHCP habilitado? (s/n): n
IP: 192.168.10.20
Máscara: 255.255.255.0
Gateway: 192.168.10.1
DNS Primario (opcional): 8.8.4.4

✔ Dispositivo creado correctamente.
DispositivoIoT y ConfiguracionRed creados exitosamente en transacción
```

### Validaciones Automáticas

El sistema valida:

- ✅ **Serial único**: Formato `SYYYY-NNN` (Ej: `S2024-001`)
- ✅ **Firmware**: Formato `vX.Y.Z` (Ej: `v1.2.3`)
- ✅ **IPs**: Formato IPv4/IPv6 válido
- ✅ **DHCP**: Si está deshabilitado, requiere IP, máscara y gateway
- ✅ **Relación 1→1**: Cada dispositivo debe tener exactamente una configuración de red

---

## 📊 Modelo de Datos (Diagrama UML)

```
┌──────────────────────────────┐          ┌─────────────────────────────┐
│    DispositivoIoT (A)        │          │   ConfiguracionRed (B)      │
├──────────────────────────────┤   1:1    ├─────────────────────────────┤
│ - id: Long                   │◆────────│ - id: Long                   │
│ - serial: String (UNIQUE)    │          │ - dhcpHabilitado: Boolean   │
│ - modelo: String             │          │ - ip: String                │
│ - ubicacion: String          │          │ - mascara: String           │
│ - firmwareVersion: String    │          │ - gateway: String           │
│ - eliminado: Boolean         │          │ - dnsPrimario: String       │
│ - configuracionRed: Config.. │          │ - eliminado: Boolean        │
│ - fechaCreacion: Timestamp   │          │ - fechaCreacion: Timestamp  │
│ - fechaModificacion: Times.. │          │ - fechaModific..: Timestamp │
└──────────────────────────────┘          └─────────────────────────────┘
```

**Relación**: DispositivoIoT posee **exactamente una** ConfiguracionRed (navegación unidireccional A→B).

Ver diagrama completo en: `docs/diagrama_uml.png`

---

## 🎯 Decisiones de Diseño

### 1. Elección del Dominio

Se eligió el dominio de **Dispositivos IoT con Configuración de Red** porque:

- Representa un caso de uso real en sistemas embebidos e industria 4.0
- Permite demostrar relaciones 1→1 de forma natural (cada dispositivo tiene una única configuración de red)
- Facilita la implementación de validaciones complejas (formatos de IP, seriales únicos)
- Es escalable a relaciones más complejas (1→N con sensores, N→M con usuarios)

### 2. FK Única vs PK Compartida

**Decisión**: Se usó **Foreign Key única** en lugar de Primary Key compartida.

**Justificación**:

| Aspecto | FK Única (Elegida) | PK Compartida |
|---------|-------------------|---------------|
| **Flexibilidad** | ✅ Permite cambiar la relación fácilmente | ❌ Estructura rígida |
| **Legibilidad** | ✅ Más intuitivo para desarrolladores | ❌ Confuso para principiantes |
| **ORM Compatibility** | ✅ Compatible con Hibernate/JPA | ⚠️ Requiere configuración especial |
| **Integridad** | ✅ Garantiza unicidad con UNIQUE constraint | ✅ Garantiza unicidad inherente |
| **Performance** | ✅ Mismo rendimiento | ✅ Mismo rendimiento |
| **Mantenimiento** | ✅ Más fácil de modificar | ❌ Difícil de refactorizar |

**Ejemplo en código**:

```sql
-- FK Única (implementada)
CREATE TABLE dispositivo_iot (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    configuracion_red_id BIGINT UNIQUE,  -- ✅ UNIQUE garantiza 1→1
    CONSTRAINT fk_config FOREIGN KEY (configuracion_red_id) 
        REFERENCES configuracion_red(id)
);

-- PK Compartida (alternativa no elegida)
CREATE TABLE dispositivo_iot (
    id BIGINT PRIMARY KEY,  -- Mismo ID que configuracion_red
    CONSTRAINT fk_config FOREIGN KEY (id) 
        REFERENCES configuracion_red(id)
);
```

### 3. Arquitectura en Capas

```
[Presentación: AppMenu]
         ↓
[Servicio: DispositivoIotService] ← Validaciones + Transacciones
         ↓
[DAO: DispositivoIotDao] ← Consultas SQL
         ↓
[Base de Datos: MySQL/PostgreSQL]
```

**Ventajas**:
- Separación de responsabilidades
- Facilita el testing (se puede mockear cada capa)
- Permite cambiar la BD sin afectar la lógica de negocio
- Reutilización de código (GenericService, GenericDAO)

---

## 📹 Video de Demostración

🎥 https://www.youtube.com/watch?v=fnXGVuiqEHk


---

## 📄 Documentación Adicional

### Informe Técnico

El informe completo (6-8 páginas) se encuentra en: `docs/informe_tecnico.pdf`

**Contenido del informe**:

1. **Integrantes del equipo** y roles asignados
2. **Elección del dominio** y justificación
3. **Diseño de la base de datos** (diagrama E-R y relacional)
4. **Decisiones de diseño** (1→1, FK vs PK, arquitectura)
5. **Implementación técnica** (clases, métodos, transacciones)
6. **Validaciones de negocio** implementadas
7. **Pruebas realizadas** y resultados
8. **Conclusiones** y mejoras futuras

---

## 🧪 Casos de Prueba

### Prueba 1: Crear Dispositivo con Validaciones

```
Input:
- Serial: "ABC123" (formato inválido)

Output:
❌ Error: Formato de serial inválido. Use formato: SYYYY-NNN

Input:
- Serial: "S2024-005" ✅
- Modelo: "Sensor-Temp"
- DHCP: false
- IP: "999.999.999.999" (IP inválida)

Output:
❌ Error: Formato de IP invalido: 999.999.999.999
```

### Prueba 2: Transaccionalidad (Rollback)

```
Escenario: Crear dispositivo con ConfiguracionRed inválida

1. Se crea ConfiguracionRed
2. Se detecta error al crear DispositivoIoT
3. ✅ ROLLBACK automático → ConfiguracionRed NO se guarda
4. Base de datos permanece consistente
```

### Prueba 3: Unicidad de Serial

```
Input:
- Serial: "S2024-001" (ya existe)

Output:
❌ Error: Ya existe un DispositivoIoT con el serial: S2024-001
```

---

## 🚀 Mejoras Futuras

- [ ] **API REST**: Exponer operaciones CRUD mediante endpoints HTTP
- [ ] **Frontend Web**: Interfaz gráfica con React/Angular
- [ ] **Autenticación**: Sistema de usuarios con roles (admin, operador)
- [ ] **Logs**: Sistema de auditoría de cambios
- [ ] **Reportes**: Generación de reportes PDF/Excel
- [ ] **Notificaciones**: Alertas por email/SMS ante eventos críticos
- [ ] **Dashboard**: Visualización de métricas en tiempo real
- [ ] **Internacionalización**: Soporte multiidioma (i18n)

---

## 📝 Licencia

Este proyecto es de uso académico para la materia de Programación 2 - UTN
