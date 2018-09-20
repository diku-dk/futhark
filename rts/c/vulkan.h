/* The simple Vulkan runtime framework used by Futhark. */

#include "vulkan/vulkan.h"
#include <stdlib.h>

#define VULKAN_SUCCEED(e) vulkan_succeed(e, #e, __FILE__, __LINE__)

struct vulkan_config {
  uint32_t api_version;
  int debugging;
  int logging;
  int preferred_device_index;
  int preferred_device_queue_index;

  const char* dump_program_to;
  const char* load_program_from;
};

void vulkan_config_init(struct vulkan_config *cfg) {
  cfg->api_version = VK_MAKE_VERSION(1, 0, 9);
  cfg->debugging = 0;
  cfg->logging = 0;
  cfg->preferred_device_index = -1;
  cfg->preferred_device_queue_index = -1;
  cfg->dump_program_to = NULL;
  cfg->load_program_from = NULL;
}

struct vulkan_context {
  VkInstance instance;
  VkPhysicalDevice physical_device;
  VkDevice device;
  uint32_t queue_family_index;

  struct vulkan_config cfg;

  size_t lockstep_width;
};

// Error codes taken directly from specification
// https://www.khronos.org/registry/vulkan/specs/1.1-extensions/man/html/VkResult.html
static const char* vulkan_error_string(unsigned int err)
{
    switch (err) {
        case VK_SUCCESS:                            return "Command successfully completed";
        case VK_NOT_READY:                          return "A fence or query has not yet completed";
        case VK_TIMEOUT:                            return "A wait operation has not completed in the specified time";
        case VK_EVENT_SET:                          return "An event is signaled";
        case VK_EVENT_RESET:                        return "An event is unsignaled";
        case VK_INCOMPLETE:                         return "A return array was too small for the result";
        case VK_ERROR_OUT_OF_DEVICE_MEMORY :        return "A device memory allocation has failed";
        case VK_ERROR_INITIALIZATION_FAILED:        return "Initialization of an object could not be completed for implementation-specific reasons";
        case VK_ERROR_DEVICE_LOST:                  return "The logical or physical device has been lost";
        case VK_ERROR_MEMORY_MAP_FAILED:            return "Mapping of a memory object has failed";
        case VK_ERROR_LAYER_NOT_PRESENT:            return "A requested layer is not present or could not be loaded";
        case VK_ERROR_EXTENSION_NOT_PRESENT:        return "A requested extension is not supported";
        case VK_ERROR_FEATURE_NOT_PRESENT:          return "A requested feature is not supported";
        case VK_ERROR_INCOMPATIBLE_DRIVER:          return "The requested version of Vulkan is not supported by the driver or is otherwise incompatible for implementation-specific reasons";
        case VK_ERROR_TOO_MANY_OBJECTS:             return "Too many objects of the type have already been created";
        case VK_ERROR_FORMAT_NOT_SUPPORTED:         return "A requested format is not supported on this device";
        case VK_ERROR_FRAGMENTED_POOL:              return "A pool allocation has failed due to fragmentation of the pool’s memory";
        case VK_ERROR_OUT_OF_POOL_MEMORY:           return "A pool memory allocation has failed";
        case VK_ERROR_INVALID_EXTERNAL_HANDLE:      return "An external handle is not a valid handle of the specified type";
        case VK_ERROR_SURFACE_LOST_KHR:             return "A surface is no longer available";
        case VK_ERROR_NATIVE_WINDOW_IN_USE_KHR:     return "The requested window is already in use by Vulkan or another API in a manner which prevents it from being used again";
        case VK_SUBOPTIMAL_KHR:                     return "A swapchain no longer matches the surface properties exactly, but can still be used to present to the surface successfully";
        case VK_ERROR_OUT_OF_DATE_KHR:              return "A surface has changed in such a way that it is no longer compatible with the swapchain, and further presentation requests using the swapchain will fail";
        case VK_ERROR_INCOMPATIBLE_DISPLAY_KHR:     return "The display used by a swapchain does not use the same presentable image layout, or is incompatible in a way that prevents sharing an image";
        case VK_ERROR_INVALID_SHADER_NV:            return "One or more shaders failed to compile or link";
        case VK_ERROR_FRAGMENTATION_EXT:            return "A descriptor pool creation has failed due to fragmentation";
        default:                                    return "Unknown";
    }
}

static void vulkan_succeed(unsigned int ret,
                           const char *call,
                           const char *file,
                           int line) {
  if (ret != VK_SUCCESS) {
    panic(-1, "%s:%d: Vulkan call\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, vulkan_error_string(ret));
  }
}

static VkPhysicalDevice get_preferred_physical_device(const struct vulkan_config *cfg, VkInstance instance) {
  uint32_t device_count = 0;
  VULKAN_SUCCEED(vkEnumeratePhysicalDevices(instance, &device_count, 0));

  VkPhysicalDevice *const devices = (VkPhysicalDevice *)malloc(sizeof(VkPhysicalDevice) * device_count);

  VULKAN_SUCCEED(vkEnumeratePhysicalDevices(instance, &device_count, devices));

  VkPhysicalDevice device;
  if (cfg->preferred_device_index >= 0) {
    if(cfg->preferred_device_index >= device_count)
      panic(1, "Specified device index not found.\n");

    VkPhysicalDeviceProperties properties;
    vkGetPhysicalDeviceProperties(devices[cfg->preferred_device_index], &properties);

    if (properties.apiVersion < cfg->api_version)
      panic(1, "Device at specified index does not support required Vulkan API.");
    
    device = devices[cfg->preferred_device_index];
  }
  else if(!get_suitable_physical_device(cfg, devices, device_count, &device)) {
    panic(1, "Could not find acceptable Vulkan device.\n");
  }

  free(devices);
  return device;
}

static int get_suitable_physical_device(const struct vulkan_config *cfg, VkPhysicalDevice *const devices,
                               uint32_t device_count, VkPhysicalDevice* device) {
  for (uint32_t i = 0; i < device_count; ++i)
  {
    VkPhysicalDeviceProperties properties;
    vkGetPhysicalDeviceProperties(devices[i], &properties);

    if (properties.apiVersion < cfg->api_version)
      continue;

    *device = devices[i];
    return 1;
  }
  return 0;
}

static uint32_t get_preferred_queue_family(const struct vulkan_config *cfg, VkPhysicalDevice physical_device) {
  uint32_t queue_family_props_count = 0;
  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_props_count, 0);

  VkQueueFamilyProperties *const queue_family_props = (VkQueueFamilyProperties *)malloc(
    sizeof(VkQueueFamilyProperties) * queue_family_props_count);

  vkGetPhysicalDeviceQueueFamilyProperties(physical_device, &queue_family_props_count, queue_family_props);

  uint32_t queue_family_index;
  if (cfg->preferred_device_queue_index >= 0) {
    if(cfg->preferred_device_queue_index >= queue_family_props_count)
      panic(1, "Specified queue family index not found.\n");
      
      const VkQueueFlags masked_flags = (~(VK_QUEUE_TRANSFER_BIT | VK_QUEUE_SPARSE_BINDING_BIT) &
                                        queue_family_props[cfg->preferred_device_queue_index].queueFlags);

    if (VK_QUEUE_COMPUTE_BIT & masked_flags)
      panic(1, "Queue family does not support compute shaders.");
    
    queue_family_index = cfg->preferred_device_queue_index;
  }
  else if(!get_suitable_queue_family(cfg, queue_family_props, queue_family_props_count, &queue_family_index)) {
    panic(1, "Could not find acceptable device queue.\n");
  }
  
  free(queue_family_props);
  return queue_family_index;
}

static uint32_t get_suitable_queue_family(const struct vulkan_config *cfg,
                                          VkQueueFamilyProperties *const queue_props,
                                          uint32_t queue_prop_count,
                                          uint32_t *queue_family_index) {
  for (uint32_t i = 0; i < queue_prop_count; i++)
  {
      const VkQueueFlags masked_flags = (~(VK_QUEUE_TRANSFER_BIT | VK_QUEUE_SPARSE_BINDING_BIT) &
                                        queue_props[i].queueFlags);

      if (VK_QUEUE_COMPUTE_BIT & masked_flags)
      {
          *queue_family_index = i;
          return 1;
      }
  }
  
  return 0;
}

static int setup_vulkan(vulkan_context *ctx, uint32_t shader[], uint32_t shader_size) {

  VkApplicationInfo application_info = {};
  application_info.sType              = VK_STRUCTURE_TYPE_APPLICATION_INFO;
  application_info.pNext              = 0;
  application_info.pApplicationName   = "";
  application_info.applicationVersion = 0;
  application_info.pEngineName        = "Futhark";
  application_info.engineVersion      = 0;
  application_info.apiVersion         = ctx->cfg.api_version;

  VkInstanceCreateInfo instance_create_info = {};
  instance_create_info.sType                   = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO;
  instance_create_info.pNext                   = 0;
  instance_create_info.flags                   = 0;
  instance_create_info.pApplicationInfo        = &application_info;
  instance_create_info.enabledLayerCount       = 0;
  instance_create_info.ppEnabledLayerNames     = 0;
  instance_create_info.enabledExtensionCount   = 0;
  instance_create_info.ppEnabledExtensionNames = 0;

  VULKAN_SUCCEED(vkCreateInstance(&instance_create_info, 0, &ctx->instance));

  ctx->physical_device = get_preferred_physical_device(&ctx->cfg, ctx->instance);

  ctx->queue_family_index = get_preferred_queue_family(&ctx->cfg, ctx->physical_device);

  const float queue_prio = 1.0f;
  VkDeviceQueueCreateInfo device_queue_create_info = {};
  device_queue_create_info.sType            = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO;
  device_queue_create_info.pNext            = 0;
  device_queue_create_info.flags            = 0;
  device_queue_create_info.queueFamilyIndex = ctx->queue_family_index;
  device_queue_create_info.queueCount       = 1;
  device_queue_create_info.pQueuePriorities = &queue_prio;

  VkDeviceCreateInfo device_create_info = {};
  device_create_info.sType                   = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO;
  device_create_info.pNext                   = 0;
  device_create_info.flags                   = 0;
  device_create_info.queueCreateInfoCount    = 1;
  device_create_info.pQueueCreateInfos       = &device_queue_create_info;
  device_create_info.enabledLayerCount       = 0;
  device_create_info.ppEnabledLayerNames     = 0;
  device_create_info.enabledExtensionCount   = 0;
  device_create_info.ppEnabledExtensionNames = 0;
  device_create_info.pEnabledFeatures        = 0;

  VULKAN_SUCCEED(vkCreateDevice(ctx->physical_device, &device_create_info, 0, &ctx->device));

  VkShaderModuleCreateInfo shader_module_create_info = {};
  shader_module_create_info.sType    = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO;
  shader_module_create_info.pNext    = 0;
  shader_module_create_info.flags    = 0;
  shader_module_create_info.codeSize = shader_size;
  shader_module_create_info.pCode    = shader;

  VkShaderModule shader_module;
  VULKAN_SUCCEED(vkCreateShaderModule(ctx->device, &shader_module_create_info, 0, &shader_module));
}

static int cleanup_vulkan(vulkan_context *ctx) {
  vkDestroyDevice(ctx->device, 0);
}